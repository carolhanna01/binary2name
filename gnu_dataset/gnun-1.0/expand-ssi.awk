# expand-ssi.awk: process Apache SSI directives.

# Copyright (C) 2015 Free Software Foundation, Inc.

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
# along with GNUnited Nations.  If not, see <http://www.gnu.org/licenses/>.

# The implementation is very limited,
# e.g. `&&' and `||' in `if' expressions are not supported, as well as
# many cases of quoting; CGI includes would be expanded in a wrong way.

# Fallback for awks who don't support RT.
BEGIN { RT = "\n" }

# Read whole file to `text'.
{ text = text $0 RT }

# Look for the variable first in the list of the named ones,
# then in the numbered parts of the latest regex.
function get_apache_var(var)
{
  if(var in apache_variables)
    return apache_variables[var]
  if(var in matched_array && var ~ /^[0-9]$/)
    return matched_array[var]
  return val = ""
}

# Substitute the variables in `str' with their values;
# return the result.
function expand_var_value(str,
  vars, n, i, begin, end, rest, var, val)
{
  n = split(str, vars, /\$/)
  if(!n)
    return ""
  str = vars[1];
  for(i = 2; i <= n; i++)
    {
      if(substr(vars[i], 1, 1) == "{")
        {
          begin = 2; end = index(vars[i], "}"); rest = end + 1
        }
      else
        {
          begin = 1; end = match(vars[i] " ", /[^0-9a-zA-Z_]/); rest = end
        }
      if(end <= begin)
        {
          str = str "$" vars[i]; continue
        }
      var = substr(vars[i],begin,end - begin)
      val = get_apache_var(var)
      str = str val (rest <= length(vars[i])? substr(vars[i], rest): "")
    }
  return str
}

# Remove the first and the last character.
function unquote(str)
{
  return substr(str, 2, length(str) - 2)
}

# Process `<!--#set ... -->' directive.
function assign_var(var, val,
  i, q, n, expanded_val)
{
  # Extract variable name and value.
  var = unquote(var)
  q = substr(val, 1, 1)
  val = unquote(val)
  if(q == "\"")
    gsub(/\\"/, q, val)
  if(q == "'")
    gsub(/\\'/, q, val)
  # Handle escaped `$'s when expanding val.
  # Note: the unescaping is done in a different way when
  # expanding `if' expressions in eval_expression().
  n = split(val, arr, /\\\$/)
  if(!n)
    expanded_val = ""
  else
    {
      expanded_val = expand_var_value(arr[1])
      for(i = 2; i <= n; i++)
        expanded_val = expanded_val "$" expand_var_value(arr[i])
    }
  apache_variables[var] = expanded_val
}

# URL encoding.
function encode_url(str)
{
  gsub(/%/, "%25", str); # This substitution must be applied first.
  gsub(/\t/, "%09", str); gsub(/\n/, "%0a", str);  gsub(/\r/, "%0d", str)
  gsub(/ /, "%20", str);  gsub(/"/, "%22", str);   gsub(/#/, "%23", str)
  gsub(/;/, "%3b", str);  gsub(/</, "%3c", str);   gsub(/>/, "%3e", str)
  gsub(/\?/, "%3f", str); gsub(/\[/, "%5b", str);  gsub(/\]/, "%5d", str)
  gsub(/\^/, "%5e", str); gsub(/`/, "%60", str);
  gsub(/\{/, "%7b", str);  gsub(/[|]/, "%7c", str); gsub(/}/, "%7d", str)
  return str
}

# Entity encoding.
function encode_entity(str)
{
  gsub(/&/, "\\&amp;", str); gsub(/"/, "\\&quot;", str)
  gsub(/</, "\\&lt;", str);  gsub(/>/, "\\&gt;", str)
  return str;
}

# Process `<!--#echo ... -->' directive.
function echo_var(str, arr,
  var, val, enc)
{
  if(length(arr[2]) > 2)
    {
      var = unquote(arr[2])
      val = get_apache_var(var)
      if(1 in arr)
        {
          enc = arr[1]
          if(enc == "entity")
            val = encode_entity(val)
          else if(enc == "url")
            val = encode_url(val)
          else if(enc != "none")
            {
              print my_name "unknown encoding `" enc \
                    "' in echo directive" > "/dev/stderr"
              exit 1
            }
        }
      else
        val = encode_entity(val)
      printf("%s", val)
    }
  else
    {
      val = substr(str,  1, match(str, /[ \t\r\n]-->/))
      print my_name "couldn't process echo directive `" val "'" > "/dev/stderr"
      exit 1
    }
}

# Remove leading and trailing spaces, unquote, expand variables.
function unquoted_var(str,
  val)
{
  val = str
  sub(/^[ \t]*/, "", val)
  sub(/[ \t]*$/, "", val)
  if(val ~ /^'.*'$/)
    val = unquote(val)
  return expand_var_value(val)
}

# Substituted in MATCH3 when match doesn't support third argument.
function match_fallback(arg1, arg2, arg3)
{
  return match(arg1, arg2)
}

# Evaluate condition from `<!--#if ... -->' directive.
# Supported forms are "string", "!string", "string1 = string2",
# "string1 == string2", "string1 != string2".
# Other forms ("-A string", "string1 > string2", "expr1 && expr2" etc.)
# are not supported.
# Advanced quoting patterns like  "' '${test}' ' = '   '"
# are also not supported (use "' ${test} ' = '   '").
function eval_expression(str,
  arr, lval, rval, negate, pattern_match, res, idx)
{
  # Escaped `$'s are treated the same way as unescaped in `if' expressions.
  gsub(/\\\$/, "$", str)
  idx = index(str, "=")
  if(!idx)
    {
      # No `=': we've got either "!string" or "string" kind of expression.
      if(str ~ /^[ \t]*!/)
        return !length(unquoted_var(substr(str, index(str, "!") + 1)))
      return length(unquoted_var(str))
    }
  if(idx > 1)
    arr[1] = substr(str, 1, idx - 1)
  else
    arr[1] = ""
  arr[2] = substr(str, idx + 1)
  if(str ~ /!==/)
    {
      print my_name "bad expression `" str "'" > "/dev/stderr"
      exit 1
    }
  # "string1 == string2" or "string1 != string2" or "string1 = string2" case.
  pattern_match = 0
  negate = 0
  if(arr[2] ~ /^=/)
    {
      arr[2] = substr(arr[2], 2)
      negate = -1
    }
  sub(/^[ \t]*/, "", arr[2])
  sub(/[ \t]*$/, "", arr[2])
  if(arr[2] ~ /^\/.*\/$/)
    {
      pattern_match = 1
      arr[2] = unquote(arr[2])
    }
  if(!negate && (arr[1] ~ /!$/))
    {
      negate = 1
      sub(/.$/, "", arr[1])
    }
  lval = unquoted_var(arr[1])
  rval = arr[2]
  if(rval ~ /^'.*'$/ && !pattern_match)
    rval = unquote(rval)
  rval = expand_var_value(rval)
  if(pattern_match)
    {
      # Note: Apache manual says you can only capture parts of regexp
      # with positive matching, but in fact the same is true for negative one.
      res = match(lval, rval, matched_array)
    }
  else
    res = (lval == rval)
  if(negate > 0)
    return !res
  return res
}

# Find the end of the directive and check that
# no comment begins before the directive ends.
function directive_idx(str,
  i, idx1)
{
  i = match(str, /[ \t\r\n]-->/)
  if(i == 0)
    {
      print my_name "broken chunk `<!--#" str "': directive has no end" \
            > "/dev/stderr"
      exit 1
    }
  idx1 = index(str, "<!--")
  if(idx1 && i >= idx1)
    {
      print my_name "broken chunk `<!--#" str "' (first ` -->' at " i \
            " comes after next `<!--' at " idx1 ")" > "/dev/stderr"
      exit 1
    }
  return i + 4
}

function process_endif()
{
  if(--depth < 0)
    {
      print my_name "unexpected endif directive in `" \
            chunks[ch_i] "'" > "/dev/stderr"
      exit 1
    }
  printf("%s", substr(chunks[ch_i], idx) "")
}

# Main program.
END {
  # Don't bother about empty files.
  if(!length(text))
    exit 0
# Assign initial values.
  # Assign prefix for error messages.
  my_name = "expand-ssi.awk: "
  # Default root directory is "../.."
  if(root == "")
    root = "../.."

  # The passed_vars variable are expected to come from the command line.
  ch_n = split(passed_vars, split_vars, /;/)
  for(ch_i = 1; ch_i <= ch_n; ch_i++)
    {
      m = index(split_vars[ch_i], "=")
      value = ""
      if(m)
        {
          var_name = substr(split_vars[ch_i], 1, m - 1)
          if(m + 1 <= length(split_vars[ch_i]))
            value = substr(split_vars[ch_i], m + 1)
        }
      else
        var_name = split_vars[ch_i]
      if(length(var_name))
        apache_variables[var_name] = value
    }

  # Relative directory name for the processed file
  # (FILENAME must be relative).
  relative_dir_name = FILENAME
  # Assume that stdin comes from the current directory,
  # although it is not very probable.
  if (relative_dir_name == "-")
    relative_dir_name = "./foo.html"
  if (relative_dir_name ~ /\//)
    sub (/[^\/]*$/, "", relative_dir_name)
  else
    relative_dir_name = root
  sub (/\/*$/, "/", relative_dir_name)

# Split the text by Apache directives.
  ch_n = split(text, chunks, "<!--#");
  depth = 0 # The current depth of `if' nesting.
# Process directives.
  if(ch_n > 0)
    printf("%s", chunks[1])
  for(ch_i = 2; ch_i <= ch_n; ch_i++)
    {
      idx = directive_idx(chunks[ch_i])
# `Set' directive: assign a variable.
      directive = substr(chunks[ch_i], 1, idx - 1)
      if(directive \
~ /^set[ \t\r\n]+var=('.+'|".+")[ \t\r\n]+value=('.*'|".*")[ \t\r\n]+-->/)
        {
          pattern[1] = directive
          sub(/^set[ \t\r\n]+var=/, "", pattern[1])
          sub(/[ \t\r\n]+value=('.*'|".*")[ \t\r\n]+-->.*/,
              "", pattern[1])
          pattern[2] = directive
          sub(/^set[ \t\r\n]+var=('.+'|".+")[ \t\r\n]+value=/, \
              "", pattern[2])
          sub(/[ \t\r\n]+-->.*/, "", pattern[2])
          assign_var(pattern[1], pattern[2])
          if(idx <= length(chunks[ch_i]))
            printf("%s", substr(chunks[ch_i], idx) "")
          continue
        }
# `Set' directive (different order of attributes).
      if(directive \
~ /^set[ \t\r\n]+value=('.*'|".*")[ \t\r\n]+var=('.+'|".+")[ \t\r\n]+-->/)
        {
          pattern[1] = directive
          sub(/^set[ \t\r\n]+value=/, "", pattern[1])
          sub(/[ \t\r\n]+var=('.*'|".*")[ \t\r\n]+-->.*/,
              "", pattern[1])
          pattern[2] = directive
          sub(/^set[ \t\r\n]+value=('.+'|".+")[ \t\r\n]+var=/, \
              "", pattern[2])
          sub(/[ \t\r\n]+-->.*/, "", pattern[2])
          assign_var(pattern[2], pattern[1])
          if(idx <= length(chunks[ch_i]))
            printf("%s", substr(chunks[ch_i], idx) "")
          continue
        }
# `Echo' directive: output a variable.
      if(chunks[ch_i] \
~ /^echo[ \t\r\n]+(encoding=("[^\"]*"|'[^']*')[ \t\r\n]+)?var=('[^']+'|"[^\"]+")[ \t\r\n]+-->/)
        {
          if(chunks[ch_i] \
~ /^echo[ \t\r\n]+(encoding=("[^\"]*"|'[^']*')[ \t\r\n]+)/)
            {
              pattern[1] = chunks[ch_i]
              sub(/^echo[ \t\r\n]+encoding=/, "", pattern[1])
              idx1 = index(substr(pattern[1], 2), substr(pattern[1], 1, 1))
              pattern[1] = substr(pattern[1], 2, idx1 - 1)
            }
          else
            delete pattern[1]
          pattern[2] = chunks[ch_i]
          sub(\
/^echo[ \t\r\n]+(encoding=("[^\"]*"|'[^']*')[ \t\r\n]+)?var=/, \
"", pattern[2])
          idx1 = index(substr(pattern[2], 2), substr(pattern[2], 1, 1))
          pattern[2] = substr(pattern[2], 1, idx1 + 1)
          echo_var(chunks[ch_i], pattern)
          if(idx <= length(chunks[ch_i]))
            printf("%s", substr(chunks[ch_i], idx) "")
          continue
        }
# `Include' directive: expand the file and rearrange chunks.
      if(chunks[ch_i] ~ \
/^include[ \t\r\n]+(file|virtual)=("[^"]*"|'[^']*')[ \t\r\n]+-->/)
        {
          pattern[2] = chunks[ch_i]
          sub(/^include[ \t\r\n]+(file|virtual)=/, "", pattern[2])
          pattern[1] = substr(pattern[2], 1, 1)
          idx1 = index(substr(pattern[2], 2), pattern[1])
          if(idx1 > 1)
            name = substr(pattern[2], 2, idx1 - 1)
          else
            name = ""
          # Construct the real path to the file.
          # Note: relative paths actually won't work in nested includes;
          # we could track the changes of the path through assigning a path
          # to every chunk, but it doesn't seem to have much sense, since
          # it is documented that we should only use absolute paths.
          if(name ~ /^\//)
            name = root name
          else
            name = relative_dir_name name
          # Load the file.
          text = ""
          while(1)
            {
              m = getline < name
              if(m < 0)
                {
                  print my_name "can't read file `" name "': " ERRNO \
                        > "/dev/stderr"
                  exit 1
                }
              if(!m)
                break
              text = text $0 RT
            }
          close(name)
          if(!length(text))
            {
              printf("%s", substr(chunks[ch_i], idx) "")
              continue
            }
          m = split(text, next_chunks, "<!--#")
          # Append the tail of current chunk to the last chunk
          # of the included file.
          next_chunks[m] = next_chunks[m] substr(chunks[ch_i], idx)
          # Move the remaining chunks and put the included file
          # in freed space.
          if(m > 1)
            {
              for(j = ch_n; j > ch_i; j--)
                chunks[j + m - 1] = chunks[j]
              for(j = 2; j <= m ; j++)
                chunks[j + ch_i - 1] = next_chunks[j]
              ch_n += m - 1
            }
          printf("%s", next_chunks[1] "")
          continue
        } # if(match(chunks[ch_i], ...
# `If' directive: skip all branches with false conditions;
# output the branch with true condition.
      if(chunks[ch_i] ~ /^if[ \t\r\n]+expr=('.*'|".*")[ \t\r\n]+-->/)
        {
          depth++
          finish = 0
          while(ch_i <= ch_n && !finish)
            {
              exp_val = 1
              # `Else' branches are output unconditionally
              # (no matching `if'/`elif' found).
              if(chunks[ch_i] !~ /^else[ \t\r\n]+-->/)
                {
                  exp_l = index(chunks[ch_i], "expr=") + length("expr=")
                  qu = substr(chunks[ch_i], exp_l, 1)
                  expr = substr(chunks[ch_i], exp_l + 1)
                  expression = substr(expr, 1, match(expr, "[^\\\\]" qu))
                  exp_val = eval_expression(expression)
                }
              if(exp_val)
                {
                  if(idx <= length(chunks[ch_i]))
                    printf("%s", substr(chunks[ch_i], idx) "")
                  break
                }
              # Skip to next `elif', `else' or `endif'
              d = 0;
              while (++ch_i <= ch_n)
                {
                  idx = directive_idx(chunks[ch_i])
                  # Next if: increase depth.
                  if(chunks[ch_i] \
                     ~ /^if[ \t\r\n]+expr=('.*'|".*")[ \t\r\n]+-->/)
                    {
                      d++; continue
                    }
                  # `Endif': break if we are at the initial depth.
                  if(chunks[ch_i] ~ /^endif[ \t\r\n]+-->/)
                    {
                      if(d-- > 0)
                        continue
                      finish = !0; process_endif(); break
                    }
                  # `Else' or `elif': break if we are at the initial depth.
                  if(d == 0 && (chunks[ch_i] ~ /^else[ \t\r\n]+-->/ \
                     || chunks[ch_i] \
                          ~ /^elif[ \t\r\n]+expr=('.*'|".*")[ \t\r\n]+-->/))
                    break
                } # while (++ch_i <= ch_n)
            } # while(ch_i <= ch_n && !finish)
          continue
        } # if(chunks[ch_i] ~ /^if[ \t\r\n]+ ...
# `Elif' or `else' directive: skip up to endif (since the branch with
# matching expression was output when processing the `if' directive).
      if((chunks[ch_i] ~ /^else[ \t\r\n]+-->/) \
          || (chunks[ch_i] ~ /^elif[ \t\r\n]+expr=('.*'|".*")[ \t\r\n]+-->/))
        {
          d = 0; chunk_i = ch_i
          while (++ch_i <= ch_n)
            {
              idx = directive_idx(chunks[ch_i])
              if(chunks[ch_i] ~ /^if[ \t\r\n]+expr=('.*'|".*")[ \t\r\n]+-->/)
                d++
              if(chunks[ch_i] ~ /^endif[ \t\r\n]+-->/)
                if(d-- == 0)
                  {
                    process_endif(); break
                  }
            }
          if(chunks[ch_i] !~ /^endif[ \t\r\n]+-->/)
            {
              print my_name "couldn't find matching endif for `<!--#" \
                chunks[chunk_i] "'" > "/dev/stderr"
              exit 1
            }
          continue
        } # if((chunks[ch_i] ~ /^else[ \t\r\n]+-->/...
# `Endif' directive: decrease depth and output the rest of the chunk.
      if(chunks[ch_i] ~ /^endif[ \t\r\n]+-->/)
        {
          process_endif(); continue
        }
# A directive that must be parsed, but didn't match against its pattern.
      if(chunks[ch_i] ~ /^(if|elif|else|endif|include|set|echo)\>/)
        {
          print my_name "couldn't parse directive `<!--#" \
            substr(chunks[ch_i], 1, idx - 1) "'" > "/dev/stderr"
          exit 1
        }
      # Unknown directive: hopefully it won't influence the validity.
      printf("<!--#%s", chunks[ch_i] "")
    } # for(ch_i = 2; ch_i <= ch_n; ch_i++)
  if(depth > 0)
    {
      print my_name "some if directives are matched with no endif." \
            > "/dev/stderr"
      exit 1
    }
}
