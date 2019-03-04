# interactiveSSH package

This R package provides functionality to login over SSH
to a remote machine and execute commands on it.
Also the output of the commands can be retrieved.

## Requirements

The package uses an SSH client in the background. 
It has been tested with the OpenSSH SSH client
available in the official repository of Ubuntu 18.04.
If it is required to login via a password passed 
as argument to the login function, the `sshpass`
command-line utility must also be installed.
Due to the use of Linux specific commands to
create named pipes, the package probably works
only under Linux.

## Installation

If `git` is present,
the package can be installed via the command line by the following commands: 

```
git clone https://github.com/gschnabel/interactiveSSH.git
R CMD INSTALL interactiveSSH
```




