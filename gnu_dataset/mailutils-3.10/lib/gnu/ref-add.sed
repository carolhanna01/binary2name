/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ mailutils / mailutils /
  tb
  s/ $/ mailutils /
  :b
  s/^/# Packages using this file:/
}
