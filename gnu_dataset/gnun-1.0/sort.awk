# Copyright (C) 2016 Free Software Foundation, Inc.

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

# Sort specifically marked lists according to given language.

function assign_locale(language,
  n, lang)
{
  n = length(language)
  if (!n)
    return ""
  lang = ""
  while((getline < "/usr/local/share/gnun/sort-linguas") > 0)
    {
      if (language" " != substr($0, 1, n + 1))
        continue
      lang = substr($0, n + 2) ".UTF-8"
      break
    }
  close("/usr/local/share/gnun/sort-linguas")
  return lang
}

function sort_list(list,
  items, keys, n, i, tmp_file, cmd, keyed_items, sorted_list)
{
  n = split(list, items,
            /<!--[ \t\r\n]+GNUN-SORT-NEXT-ITEM[ \t\r\n]+-->/)
  cmd = "/bin/mktemp -t gnun-sort.XXXXXXXXXX"
  cmd | getline tmp_file
  close(cmd)
  for(i = 1; i <= n; i++)
    {
      # Extract key.
      keys[i] = items[i]
      sub(/.*<!--[ \t\r\n]+GNUN-SORT-BEGIN-KEY[ \t\r\n]+-->/, "",
          keys[i])
      sub(/<!--[ \t\r\n]+GNUN-SORT-END-KEY[ \t\r\n]+-->.*/, "",
          keys[i])
      # Remove HTML tags.
      gsub(/<[^!][^>]*>/, "", keys[i])
      gsub(/<\/[^>]*[^-]>/, "", keys[i])
      # Remove HTML comment delimiters.
      gsub(/<!--/, "", keys[i])
      gsub(/-->/, "", keys[i])
      # Trim leading and trailing whitespace.
      gsub(/^[\t\r\n ]*/, "", keys[i])
      gsub(/[\t\r\n ]*$/, "", keys[i])
      # Normalize whitespace.
      gsub(/[\t\r\n ]+/, " ", keys[i])
      if (keys[i] in keyed_items)
        keyed_items[keys[i]] = keyed_items[keys[i]] "\n"
      keyed_items[keys[i]] = keyed_items[keys[i]] items[i]
      print keys[i] >> tmp_file
    }
  close(tmp_file)
  cmd = " LC_CTYPE=" language " /usr/bin/sort " tmp_file
  cmd = "LC_ALL=" language " LC_COLLATE=" language cmd
  cmd = "LANG= LOCPATH=/usr/local/share/gnun/i18n " cmd
  i = 0
  sorted_list = ""
  while((cmd | getline) > 0)
    {
      if (i)
        sorted_list = sorted_list "<!-- GNUN-SORT-NEXT-ITEM -->"
      sorted_list = sorted_list keyed_items[$0]
      i++
    }
  printf("%s", sorted_list)

  close(cmd)
  system("rm -f " tmp_file) 
}

BEGIN {
  language = assign_locale(language)
# Fallback for awks who don't support RT.
  RT = "\n"
}

/<!--[ \t\r\n]+GNUN-SORT-START[ \t\r\n]+-->/ {
  line = $0
  idx = match(line, /<!--[ \t\r\n]+GNUN-SORT-START[ \t\r\n]+-->/)
  idx += index(substr(line, idx), ">")
  printf substr(line, 1, idx - 1)
  in_list = 1;
  list = substr(line, idx) RT
  next
}

!in_list { printf("%s", $0 RT); next }

/<!--[ \t\r\n]+GNUN-SORT-STOP[ \t\r\n]+-->/ {
  line = $0
  idx = match(line, /<!--[ \t\r\n]+GNUN-SORT-STOP[ \t\r\n]+-->/)
  if (idx > 1)
    {
      list = list substr(line, 1, idx - 1)
      line = substr(line, idx)
    }
  if (language)
    sort_list(list)
  else
    printf("%s", list)
  print line
  in_list = 0
  next
}
{ list = list $0 RT }
