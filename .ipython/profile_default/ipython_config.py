c = get_config()
c.TerminalInteractiveShell.confirm_exit = False
c.PromptManager.in_template  = '{color.Cyan}in [{color.LightCyan}{count}{color.Cyan}]: '
