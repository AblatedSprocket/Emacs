# Emacs Configuration

## Installation
The installation documentation is for Ubuntu and other distributions that use apt. I am pretty sure there are some differences in the libraries provided by other package managers. For Emacs to pick up the configuration provided, it must be placed in ```~/.emacs.d/``` if using Emacs 26, or ```/.config/emacs/``` if using Emacs 27 or newer. The installation process for both versions of Emacs (from source) is provided below.

### Emacs 26
Because this configuration uses mu4e, it's best to install from [source](http://ftp.gnu.org/gnu/emacs/), unpacking everything using ```tar -xzvf emacs-26.3.tar.gz```. 

Emacs has a lot of dependencies: ```sudo apt install libgtk-3-dev libwebkit2gtk-4.0-dev libpng-dev libgif-dev libotf-dev libxml2-dev libxpm-dev libjpeg-dev libtiff-dev libgnutls28-dev libncurses5-dev```. You will want to run a configuration script, but first execute ```./autogen.sh``` from within the Emacs directory to create the configuration script. Then run ```sudo ./configure --with-xwidgets``` to configure everything and provide information on missing dependencies. Follow this up with ```sudo make && sudo make install``` and Emacs should install.

### Emacs 27
The process used here to install Emacs 27 is slightly different. Again, download the [source code](http://git.savannah.gnu.org/cgit/emacs.git). That said, the install packages might need revising on a new installation. Here goes. First install the dependencies: ```sudo apt install libgtk-3-dev libwebkit2gtk-4.0-dev libpng-dev libgif-dev libotf-dev libxml2-dev libxpm-dev libjpeg-dev libtiff-dev libgnutls28-dev libncurses5-dev texinfo```. Run ```./autogen.sh``` to create the configuration script, then run that using ```sudo ./configure --with-json --with-modules --with-xwidgets --without-pop``` to use the new native JSON parsing and better terminal support with vterm. According to the documentation, --with-json should not be necessary, but I added it for good measure. Once complete, run ```sudo make && sudo make install``` to get everything installed.

### Emacs Setup
On first install, uncomment these lines in the ```init.el``` file:
```
(package-refresh-contents)
(package-initialize)
```
These lines can be commented out again once Emacs successfully runs.

The config in this repository provides a basic writing environment, as well as development support for Rust and Python projects. Email managements is also provided, but that is covered in its own section.

For Rust development, download latest rust-analyzer release and place in ```~/.cargo/bin```. Add executable permissions by executing ```sudo chmod +x rust-analyzer``` from within this directory. This configuration uses LSP-mode for intellisense and other IDE features. LSP won't necessarily provide any input until you build a project.

For Python development, install python language server: ```pip3 install 'python-language-server[all]'```. Again, Emacs uses Python development features via LSP-mode.

### Emacs Mail Client
If you don't want this functionality, comment the line ```(require 'init-mail)``` in ```init.el```. If you do want it, there are a few things to install to make Emacs work as a mail client. The first task is to Install mu4e, isync, html2text and gpg2 via apt: ```sudo apt install mu4e isync html2text gpg2```.

Create a file whose contents is your mail password and encrypt via gpg2: ```gpg2 -c .authinfo``` 

Remove original files and place the encrypted ones in folders with names to match the references in ```mu4e/.mbsyncrc```.

With everything installed we need to perform an initial sync using ```mbsync```. Before that, a mail directory must be create: ```mkdir ~/Mail``` 

Now, mail can be synced using the config file. Since it's in an unconventional directory, it must be specified explicitly: ```mbsync -c "~/.emacs.d/mu4e/.mbsyncrc" -a``` 

The last step is to index the messages with mu: ```mu index --maildir="~/Mail"```.

## Run Emacs Daemon
When installing from source, it appears that the ```emacs.service``` file does not get created. If it does, it doesn't get put where it needs to, so starting an Emacs server (i.e. running Emacs as a daemon) by executing ```systemctl --user enable emacs``` does not work. I created my own systemd entry in ```~/.config/systemd/user/emacs.service``` as described [here](https://www.emacswiki.org/emacs/EmacsAsDaemon):
```
[Unit]
Description=Emacs texteditor
Documentation=info:emacs man:emacs(1) https://gnu.org/software/emacs/

[Service]
Type=forking
ExecStart=/usr/local/bin/emacs --daemon
ExecStop=/usr/local/bin/emacsclient --eval "(kill-emacs)"
Environment=SSH_AUTH_SOCK=%t/keyring/ssh
Restart=on-failure

[Install]
WantedBy=default.target
```

To run an Emacs client, either run ```emacsclient -create-frame --alternate-editor=""```, or create a Desktop entry in ```.local/share/applications\emacsclient.desktop```:

```
[Desktop Entry]
Name = Emacs Client
GenericName=Text Editor
Comment=Emacs Editor
MimeType=text/english;text/plain
Exec=emacsclient -create-frame --alternate-editor=""
Icon=emacs
Type=Application
Terminal=false
Categories=Development;TextEditor;Utility;
StartupWMClass=Emacs
```
