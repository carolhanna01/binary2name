/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ libunistring / libunistring /
  tb
  s/ $/ libunistring /
  :b
  s/^/# Packages using this file:/
}
