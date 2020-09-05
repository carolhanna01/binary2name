# Cron job for processing GNATS incoming queue.

5,15,25,35,45,55 * * * * gnats test -x /usr/lib/gnats/queue-pr && /usr/lib/gnats/queue-pr --run ; exit 0
