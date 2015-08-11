module Terminal where

data Terminal = Terminal {
    termcmd :: String
  , termcd :: String
  , termexec :: String
  }

gnomeTerminal :: Terminal
gnometerminal = Terminal "gnome-terminal" "-e"

urxvtTerminal :: Terminal
urxvtTerminal = Terminal "urxvt" "-cd" "-e"

konsoleTerminal :: Terminal
konsoleTerminal = Terminal "konsole" "--workdir" "-e"
