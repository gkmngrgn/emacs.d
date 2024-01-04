local config_dir = "~/.config/emacs/"

cmd.add_task {
  name = "setup",
  description = "setup my emacs",
  command = function()
    cmd.run("mkdir -p " .. config_dir)
    cmd.copy("*.el", config_dir)
  end
}

cmd.add_task {
  name = "install",
  description = "install system dependencies",
  required_platforms = { "macos" },
  required_commands = { "npm" },
  command = function()
    cmd.brew_install({
      "fd",
      "lua-language-server",
      "multimarkdown",
      "pyenv",
      "ripgrep",
      "rust-analyzer"
    })
    local npm_packages = {
      "typescript",
      "typescript-language-server",
      "vscode-langservers-extracted",
      "yaml-language-server"
    }
    for _, package in ipairs(npm_packages) do
      cmd.run("npm i -g " .. package)
    end
  end
}
