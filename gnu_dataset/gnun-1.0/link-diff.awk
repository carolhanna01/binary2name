# Copyright (C) 2014, 2016, 2017, 2019 Free Software Foundation, Inc.

# This file is part of GNUnited Nations.

# GNUnited Nations is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# GNUnited Nations is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNUnited Nations.  If not, see <https://www.gnu.org/licenses/>.

# Output differences in sets of links and anchors in msgids and msgstrs
# as a HTML file. Exit code is 0 when no crucially unmatched things
# were found, else 1.

# To use like
# f=../licenses/gpl-faq.cv.po;time LC_ALL=C awk -v language=cv \
#    -v title=${f##*/} -f link-diff.awk <$f > t.html

BEGIN {
# The language code is used for adjusting links to CC licenses;
# creativecommons.org uses the ll_CC notation, our file suffix
# convention is ll-cc.
  if (length(language) > 2)
    language = substr(language, 1, 2) "_" toupper(substr(language, 4))
}

# Transform special characters from PO file to HTML text. 
function escape_chars(x)
{
# Unescape quotes \" -> ".
  gsub(/\\"/, "\"", x)
# Escape HTML specal characters.
  gsub(/&/, "\\&amp;", x)
  gsub(/>/, "\\&gt;", x)
  gsub(/</, "\\&lt;", x)
# Make newlines visible in a browser.
  gsub(/\n/, "<br />\n", x)
  return x
}

# Honor newlines escaped in PO msgids an mststrs.
function make_po_breaks(x)
{
  gsub(/\\n/, "<br />\n", x)
  return x
}

# Fill output array out with links from array.
# The items of array are strings like
# "http://link.org">text...</a> more text...
# The function puts http://link.org in the out array
# (the number of entries is put to out[0]).
# n is the length of the input array,
# label is only used to report where errors occur.
function list_attr(array, n, label, out,
  delimiter, j, l)
{
  delete out
  j = 1
  for (i = 2; i <= n; i++)
    {
      delimiter = substr(links[i], 1, 1)
      l = index(substr(links[i], 2), delimiter)
      if (l < 1)
        {
          print "No closing " delimiter " found in " \
                label " " links[i] > "/dev/stderr"
          continue
        }
      out[j++] = substr(links[i], 2, l - 1)
    }
  out[0] = j
}

# Like split, but also fill the idx array with the indices
# of matched strings.
function split_with_pos(txt, links, idx, regex,
  n, t, i)
{
  t = txt; n = 1; i = 1
  while (match(t, regex))
    {
      links[n] = substr(t, 1, RSTART + RLENGTH - 1)
      i += RSTART + RLENGTH - 1
      idx[n] = i + 1
      t = substr (t, RSTART + RLENGTH)
      n++
    }
  links[n] = t
  idx[n] = i
  return n
}

# Find HTML links in the text.
# txt - input text
# out - output array with links
# pos - output array of respective link positions
function get_links(txt, out, pos,
  n, spc)
{
  spc = "([ \t\r\n]|<br />\n)"
  n = split_with_pos(txt, links, pos,
      "[&]lt;a" spc "+([^&]*" spc "+?)href" spc "*=" spc "*")
  list_attr(links, n, "link", out)
}

# Find HTML ids and names in the text.
# txt - input text
# out - output array with ids
# pos - output array of respective id positions
function get_ids(txt, out, pos,
  n, spc)
{
  spc = "([ \t\r\n]|<br />\n)"
  n = split_with_pos(txt, links, pos,
      "[&]lt;[^&]*" spc "+(id|name)" spc "*=" spc "*")
  list_attr(links, n, "id", out)
}

# Compile difference between the set of links in msgid (orig_links
# at orig_pos) and the set of links in translations (trans_links
# at trans_pos).  Links to CC licenses are adjusted according to
# language code lang.
# The return value is a diff string like
# -pos0:link0
# +pos1:link1
function diff_attr_set(orig_links, orig_pos, trans_links, trans_pos, lang,
  i, j, k, vex, out, msgid_links)
{
  msgid_links[0] = orig_links[0]
  for (i = 0; i < orig_links[0]; i++)
    {
      msgid_links[i] = orig_links[i]
      if (!lang)
        continue
      if (msgid_links[i] ~ \
/^(http(s?):?)\/\/creativecommons.org\/(licenses\
\/by((-(nc-nd|nd|sa))?)\/(3\.0((\/(us|nz))?)|4\.0))\
|(publicdomain\/zero\/1\.0)\/$/)
         msgid_links[i] = msgid_links[i] "deed." lang
    }
  i = 1; j = 1
  out = ""
  while (i < msgid_links[0] && j < trans_links[0])
    {
      id_link = msgid_links[i]
      if (verbose_diff)
        out = out "&:" i "[" orig_pos[i] "]:" j "[" trans_pos[j] "]:" \
              orig_links[i] ":" trans_links[j] "\n"
      if (msgid_links[i] == trans_links[j])
        {
          if (verbose_diff)
            out = out "0:" orig_pos[i] ":" trans_pos[j] ":" \
                  orig_links[i] "\n"
          i++; j++
          continue
        }
      vex = 0
      for (k = 1; i + k < msgid_links[0]; k++)
        if (msgid_links[i + k] == trans_links[j])
          {
            vex = 1
            while (k--)
              {
                out = out "-" orig_pos[i] ":" orig_links[i] "\n"
                i++
              }
            break
          }
      if (!vex)
        for (k = 1; j + k < trans_links[0]; k++)
          if (msgid_links[i] == trans_links[j + k])
            {
              vex = !0
              while (k--)
                {
                  out = out "+" trans_pos[j] ":" trans_links[j] "\n"
                  j++
                }
              break
            }
      if (!vex)
        {
          out = out "-" orig_pos[i] ":" orig_links[i] "\n"
          i++
          out = out "+" trans_pos[j] ":" trans_links[j] "\n"
          j++
        }
    }
  while (i < msgid_links[0])
    {
      out = out "-" orig_pos[i] ":" orig_links[i] "\n"
      i++
    }
  while (j < trans_links[0])
    {
      out = out "+" trans_pos[j] ":" trans_links[j] "\n"
      j++
    }
  if (out != "") # Strip trailing newline.
    out = substr(out, 1, length(out) - 1)
  return out
}

# Assign link_diff and id_diff items for the next PO entry.
# link_diff is difference in the set of links,
# id_diff is difference in the set of ids of original string vs translation.
function process_entry( \
  n, msgid_links, trans_links, msgid_pos, trans_pos, i)
{
  n = msg_no++
  comments[n] = escape_chars(substr(comment, 2)); comment = ""
  msg_ids[n] = make_po_breaks(escape_chars(msgid)); msgid = ""
  msg_strs[n] = make_po_breaks(escape_chars(msgstr)); msgstr = ""
  link_diff[n] = id_diff[n] = ""
  if (msg_ids[n] == "")
    return
  get_links(msg_ids[n], msgid_links, msgid_pos)
  get_links(msg_strs[n], trans_links, trans_pos)
  link_diff[n] = diff_attr_set(msgid_links, msgid_pos, trans_links,
                               trans_pos, language)
  get_ids(msg_ids[n], msgid_links, msgid_pos)
  get_ids(msg_strs[n], trans_links, trans_pos)
  id_diff[n] = diff_attr_set(msgid_links, msgid_pos, trans_links, trans_pos, "")
}

# Parse difference diff to array of positions pos and array of links
# links, filtering the differences starting with one-letter prefix.
function parse_diff(diff, pos, links, prefix,
  i, j, n, p, t)
{
  n = split(diff, t, "\n")
  j = 0
  for (i = 1; i <= n; i++)
    {
      if (substr(t[i], 1, 1) != prefix)
        continue
      p = index(t[i], ":")
      pos[j] = substr(t[i], 2, p - 2) - 1
      links[j++] = substr(t[i], p + 1)
    }
  return j
}

# Merge two difference strings to single string with
# prefix '0' for diff0 and '1' for diff1.
function merge_diffs(diff0, diff1, prefix,
  i0, i1, n0, n1, pos0, pos1, links0, links1, sum)
{
  n0 = parse_diff(diff0, pos0, links0, prefix)
  n1 = parse_diff(diff1, pos1, links1, prefix)
  sum = ""
  i0 = i1 = 0
  while (i0 < n0 || i1 < n1)
    if (i0 < n0 && (i1 == n1 || pos0[i0] < pos1[i1]))
      {
        sum = sum "\n0" pos0[i0] ":" links0[i0]
        i0++
      }
    else
      {
        sum = sum "\n1" pos1[i1] ":" links1[i1]
        i1++
      }
  return substr(sum, 2)
}

# Return text highlighted according to merged difference diff
# using HTML tags given in the tags array.
function highlight(text, diff, tags,
  i, t_idx, n, t, a, pos, len, p, out)
{
  n = split(diff, t, "\n")
  a = 0; out = ""
  for (i = 1; i <= n; i++)
    {
      idx = substr(t[i], 1, 1)
      p = index(t[i], ":")
      pos = substr(t[i], 2, p - 2) + a
      len = length(t[i]) - p
      text = substr(text, 1, pos) "<" tags[idx] ">" substr(text, pos + 1, len) \
            "</" tags[idx] ">" substr(text, pos + len + 1)
      a = a + length("<" tags[idx] ">" "</" tags[idx] ">")
    }
  return text
}

# Get link text from difference notation.
function extract_link(text)
{
  return substr(text, index(text, ":") + 1)
}

# Suppress certain differences:
# * Local links from translations to ids defined in translator's notes
#   are allowed.
# * Local (back) links from translator's notes to ids additionally
#   defined in translations are allowed.
# * Links from translator's credits are allowed.
# * Pairs of links that exchange their order, like
#   <a href="rms">rms</a>'s <a href="lec">lecture</a> ->
#   -> <a href="lec">lecture</a> by <a href="rms">rms</a>
function pacify_translator_notes(\
 i, j, n, m, trans_notes, back_trans_notes, tn, tn_i, t, link, k)
{
  trans_notes = ""
  back_trans_notes = ""
  # Extract translator's notes; drop any links in translator's credits.
  for (i = 0; i < msg_no; i++)
    if (msg_ids[i] == "*GNUN-SLOT: TRANSLATOR'S NOTES*")
      {
        trans_notes = id_diff[i]
        back_trans_notes = link_diff[i]
        gsub(/(^|\n)[+][^:]*:[^#][^\n]*/, "", back_trans_notes)
        gsub(/(^|\n)[-][^:]*:[^\n]*/, "", back_trans_notes)
        gsub(/\n\n+/, "\n", back_trans_notes)
        gsub(/(^|\n)[+][^:]*:[#][^\n]*/, "", link_diff[i])
        gsub(/\n\n+/, "\n", link_diff[i])
        tn_i = i
      }
    else if (msg_ids[i] == "*GNUN-SLOT: TRANSLATOR'S CREDITS*")
      link_diff[i] = ""
  # Suppress pairs of links that changed their order in the text by one.
  for (i = 0; i < msg_no; i++)
    {
      if (link_diff[i] == "")
        continue
      n = split(link_diff[i], t, "\n")
      if (n < 2)
        continue
      link = ""
      for (j = 1; j < n; j++)
        {
          if (substr(t[j], 1, 1) != "-")
            {
              link = link "\n" t[j]
              continue
            }
          if (substr(t[j + 1], 1, 1) != "+")
            {
              link = link "\n" t[j]
              continue
            }
          if (extract_link(t[j]) == extract_link(t[j + 1]))
            {
              j++
              continue
            }
          link = link "\n" t[j]
        }
      if (j == n)
        link = link "\n" t[j]
      link_diff[i] = substr(link, 2)
    }
  if (trans_notes == "")
    return
  # Suppress links to translator's notes.
  n = split(trans_notes, tn, "\n")
  for (i = 1; i <= n; i++)
    sub(/[^:]*:/, "", tn[i])
  for (i = 0; i < msg_no; i++)
    {
      m = split(link_diff[i], t, "\n")
      for (j = 1; j <= m; j++)
        {
          if (substr(t[j], 1, 1) != "+")
            continue
          link = extract_link(t[j])
          for (k = 1; k <= n; k++)
            if ("#" tn[k] == link)
              {
                t[j] = ""
                break
              }
        }
      link_diff[i] = ""
      for (j = 1; j <= m; j++)
        if (t[j] != "")
          link_diff[i] = link_diff[i] "\n" t[j]
      link_diff[i] = substr(link_diff[i], 2)
    }
  # Suppress new local links from translator's notes.
  n = split(back_trans_notes, tn, "\n")
  for (i = 1; i <= n; i++)
    sub(/[^:]*:/, "", tn[i])
  for (i = 0; i < msg_no; i++)
    {
      m = split(id_diff[i], t, "\n")
      for (j = 1; j <= m; j++)
        {
          if (substr(t[j], 1, 1) != "+")
            continue
          link = extract_link(t[j])
          for (k = 1; k <= n; k++)
            if (tn[k] == "#" link)
              {
                t[j] = ""
                break
              }
        }
      id_diff[i] = ""
      for (j = 1; j <= m; j++)
        if (t[j] != "")
          id_diff[i] = id_diff[i] "\n" t[j]
      id_diff[i] = substr(id_diff[i], 2)
    }
  id_diff[tn_i] = ""
}

# Count the number of unmatched links; differences in mailto: are skipped.
function count_link_diffs(\
  i, j, n, cnt, d)
{
  cnt = 0
  for (i = 0; i < msg_no; i++)
    {
      n = split(link_diff[i], d, "\n")
      for (j = 1; j <= n; j++)
        if (d[j] !~ /[^:]*:mailto:/)
          cnt++
    }
  return cnt
}

# Count the number of unmatched ids.
function count_id_diffs(\
  i, cnt, d)
{
  cnt = 0
  for (i = 0; i < msg_no; i++)
    cnt += split(id_diff[i], d, "\n")
  return cnt
}

# Start parsing msgid.
/^msgid "/ {
  state = "msgid"
  sub(/^msgid "/, "")
  sub(/"[ \t\r\n]*$/, "")
  msgid = $0
  next
}

# Start parsing translation.
/^msgstr "/ {
  state = "msgstr"
  sub(/^msgstr "/, "")
  sub(/"[ \t\r\n]*$/, "")
  msgstr = $0
  next
}

# Empty string: put entry into arrays.
/^[ \t\r\n]*$/ {
  process_entry()
  next
}

# Skip specific PO4A comments.
/^# type: (Content|Attribute .*) of: / { next }

# Parse comment.
/^#( |[.] TRANSLATORS)/ {
  c = $0
  sub(/#( |[.] )/, "", c)
  comment = comment "\n" c
  next
}

# Parse multiline item (msgid or translation).
/^"/ {
  sub(/^"/, "")
  sub(/"[ \t\r\n]*$/, "")
  if(state == "msgid")
    msgid = msgid $0
  else
    msgstr = msgstr $0
}

# Nothing useful should be left; skip, if anything.
{ next }

# Output HTML.
END {
  if(msgid != "")
    process_entry()
  pacify_translator_notes()
  output = "<html><head>\n"
  output = output \
"<meta http-equiv='content-type' content='text/html; charset=utf-8' />\n"
  if (title)
    output = output "<title>" title "</title>\n"
  output = output "<style type='text/css' media='print,screen'>\n<!--\n"
  output = output ".select, td strong.select { background: springgreen; }\n"
  output = output "td strong { background: lightsalmon; }\n"
  output = output "td em { background: aqua; }\n"
  output = output "td.com { background: springgreen; }\n"
  output = output "-->\n</style>\n"
  output = output "</head><body>\n"
  if (title)
    output = output "<h2>" title "</h2>\n"
  l_cnt = count_link_diffs()
  i_cnt = count_id_diffs()
  output = output "<p>Mismatched links: "  l_cnt ".</p>\n"
  output = output "<p>Mismatched ids: "  i_cnt ".</p>\n"
  output = output "<table border='1'>\n"
  output = output "<tr><th>#</th><th>text</th>\n"
  tags[0] = "strong"
  tags[1] = "em"
  for (i = 0; i < msg_no; i++)
    {
      if (!(link_diff[i] || id_diff[i] || output_all))
        continue
      i_text = i
      tr_class = ""
      if (id_diff[i] || link_diff[i])
        {
          i_text = "<strong class='select'>" i_text "</strong>"
          tr_class = " class='select'"
        }
      output = output "<tr><td" tr_class " rowspan='3'>" i_text "</td>\n"
      output = output "<td class='com'>\n"
      output = output comments[i] "&nbsp;\n"
      output = output "</td>\n</tr>\n<tr><td class='msgid'>\n"
      diff = merge_diffs(link_diff[i], id_diff[i], "-")
      output = output highlight(msg_ids[i], diff, tags) "&nbsp;\n"
      output = output "</td>\n</tr>\n<tr><td class='msgstr'>\n"
      diff = merge_diffs(link_diff[i], id_diff[i], "+")
      output = output highlight(msg_strs[i], diff, tags) "&nbsp;\n"
      output = output "</td>\n</tr>\n"
    }
  output = output "</table>\n"
  output = output "</body>"
  print output
  if (l_cnt || i_cnt)
    exit 1
  exit 0
}
