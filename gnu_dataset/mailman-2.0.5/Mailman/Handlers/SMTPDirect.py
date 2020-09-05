# Copyright (C) 1998,1999,2000 by the Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

"""Local SMTP direct drop-off.

This module delivers messages via SMTP to a locally specified daemon.  This
should be compatible with any modern SMTP server.  It is expected that the MTA
handles all final delivery.  We have to play tricks so that the list object
isn't locked while delivery occurs synchronously.

"""

import os
import string
import time
import socket

from Mailman import mm_cfg
from Mailman import Utils
from Mailman.Handlers import HandlerAPI
from Mailman.Logging.Syslog import syslog
from Mailman.pythonlib import smtplib

threading = None
try:
    if mm_cfg.MAX_DELIVERY_THREADS > 0:
        import threading
        import Queue
except ImportError:
    pass



def process(mlist, msg, msgdata):
    recips = msgdata.get('recips')
    if not recips:
        # Nobody to deliver to!
        return
    admin = mlist.GetAdminEmail()
    msgtext = str(msg)
    #
    # Split the recipient list into SMTP_MAX_RCPTS chunks.  Most MTAs have a
    # limit on the number of recipients they'll swallow in a single
    # transaction.
    if mm_cfg.SMTP_MAX_RCPTS <= 0:
        chunks = [recips]
    else:
        chunks = chunkify(recips, mm_cfg.SMTP_MAX_RCPTS)
    refused = {}
    t0 = time.time()
    # We can improve performance by unlocking the list during delivery.  We
    # must re-lock it though afterwards to ensure the pipeline delivery
    # invariant.
    try:
        mlist.Save()
        mlist.Unlock()
        if threading:
            threaded_deliver(admin, msgtext, chunks, refused)
        else:
            for chunk in chunks:
                deliver(admin, msgtext, chunk, refused)
    finally:
        t1 = time.time()
        mlist.Lock()
    # Log the successful post
    syslog('smtp', 'smtp for %d recips, completed in %.3f seconds' %
           (len(recips), (t1-t0)))

    if refused:
        # Always log failures
        syslog('post', 'post to %s from %s, size=%d, %d failures' %
               (mlist.internal_name(), msg.GetSender(), len(msg.body),
                len(refused)))
    elif msgdata.get('tolist'):
        # Log the successful post, but only if it really was a post to the
        # mailing list.  Don't log sends to the -owner, or -admin addrs.
        # -request addrs should never get here.
        syslog('post', 'post to %s from %s, size=%d, success' %
               (mlist.internal_name(), msg.GetSender(), len(msg.body)))

    # Process any failed deliveries.
    tempfailures = []
    for recip, (code, smtpmsg) in refused.items():
        # DRUMS is an internet draft, but it says:
        #
        #    [RFC-821] incorrectly listed the error where an SMTP server
        #    exhausts its implementation limit on the number of RCPT commands
        #    ("too many recipients") as having reply code 552.  The correct
        #    reply code for this condition is 452. Clients SHOULD treat a 552
        #    code in this case as a temporary, rather than permanent failure
        #    so the logic below works.
        #
        if code >= 500 and code <> 552:
            # It's a permanent failure for this recipient so register it.  We
            # don't save the list between each registration because we assume
            # it happens around the whole message delivery sequence
            mlist.RegisterBounce(recip, msg)
        else:
            # Deal with persistent transient failures by queuing them up for
            # future delivery.  TBD: this could generate lots of log entries!
            syslog('smtp-failure', '%d %s (%s)' % (code, recip, smtpmsg))
            tempfailures.append(recip)
    if tempfailures:
        msgdata['recips'] = tempfailures
        raise HandlerAPI.SomeRecipientsFailed



def chunkify(recips, chunksize):
    # First do a simple sort on top level domain.  It probably doesn't buy us
    # much to try to sort on MX record -- that's the MTA's job.  We're just
    # trying to avoid getting a max recips error.  Split the chunks along
    # these lines (as suggested originally by Chuq Von Rospach and slightly
    # elaborated by BAW).
    chunkmap = {'com': 1,
                'net': 2,
                'org': 2,
                'edu': 3,
                'us' : 3,
                'ca' : 3,
                }
    buckets = {}
    for r in recips:
        tld = None
        i = string.rfind(r, '.')
        if i >= 0:
            tld = r[i+1:]
        bin = chunkmap.get(tld, 0)
        bucket = buckets.get(bin, [])
        bucket.append(r)
        buckets[bin] = bucket
    # Now start filling the chunks
    chunks = []
    currentchunk = []
    chunklen = 0
    for bin in buckets.values():
        for r in bin:
            currentchunk.append(r)
            chunklen = chunklen + 1
            if chunklen >= chunksize:
                chunks.append(currentchunk)
                currentchunk = []
                chunklen = 0
        if currentchunk:
            chunks.append(currentchunk)
            currentchunk = []
            chunklen = 0
    return chunks



def pre_deliver(envsender, msgtext, failures, chunkq):
    while 1:
        # Get the next recipient chunk, if there is one
        try:
            recips = chunkq.get(0)
        except Queue.Empty:
            # We're done
            break
        # Otherwise, process the chunk
        deliver(envsender, msgtext, recips, failures)


def threaded_deliver(envsender, msgtext, chunks, failures):
    threads = {}
    numchunks = len(chunks)
    chunkq = Queue.Queue(numchunks)
    # Populate the queue with all the chunks that need processing.
    for chunk in chunks:
        chunkq.put(chunk)
    # Start all the threads
    for i in range(min(numchunks, mm_cfg.MAX_DELIVERY_THREADS)):
        threadfailures = {}
        t = threading.Thread(target=pre_deliver,
                             args=(envsender, msgtext, threadfailures, chunkq))
        threads[t] = threadfailures
        t.start()
    # Now wait for all the threads to complete and collate their failure
    # dictionaries.
    for t, threadfailures in threads.items():
        t.join()
        failures.update(threadfailures)
    # All threads have exited
    threads.clear()



def deliver(envsender, msgtext, recips, failures):
    refused = {}
    # Gather statistics on how long each SMTP dialog takes.
##    t0 = time.time()
    try:
        conn = smtplib.SMTP(mm_cfg.SMTPHOST, mm_cfg.SMTPPORT)
        try:
            # make sure the connect happens, which won't be done by the
            # constructor if SMTPHOST is false
            refused = conn.sendmail(envsender, recips, msgtext)
        finally:
##            t1 = time.time()
##            syslog('smtp', 'smtp for %d recips, completed in %.3f seconds' %
##                   (len(recips), (t1-t0)))
            conn.quit()
    except smtplib.SMTPRecipientsRefused, e:
        refused = e.recipients
    # MTA not responding, or other socket problems, or any other kind of
    # SMTPException.  In that case, nothing got delivered
    except (socket.error, smtplib.SMTPException), e:
        syslog('smtp', 'All recipients refused: %s' % e)
        # If the exception had an associated error code, use it, otherwise,
        # fake it with a non-triggering exception code
        errcode = getattr(e, 'smtp_code', -1)
        errmsg = getattr(e, 'smtp_error', 'ignore')
        for r in recips:
            refused[r] = (errcode, errmsg)
    failures.update(refused)
