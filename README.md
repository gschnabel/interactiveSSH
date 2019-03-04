# interactiveSSH package

This R package provides functionality to login over SSH
to a remote machine and execute commands on it.
Also the output of the commands can be retrieved.

## Requirements

The package uses an ssh client in the background. 
It has been tested with the OpenSSH SSH client
as available on Ubuntu 18.04.
If it is required to login via a password passed 
as argument to the login function, the `sshpass`
command-line utility must also be installed.

