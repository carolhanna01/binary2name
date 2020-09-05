/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ anubis / anubis /
  tb
  s/ $/ anubis /
  :b
  s/^/# Packages using this file:/
}
