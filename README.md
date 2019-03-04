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
the package can be installed via the command line by invoking the following commands: 

```
git clone https://github.com/gschnabel/interactiveSSH.git
R CMD INSTALL interactiveSSH
```

## Usage

In an R session, connecting to the server `server.com` as user `username` and using the password `password`
would be done like that: 

```
library(interactiveSSH)
sshcon <- initInteractiveSSH("username@server.com", password="password", tempdir.loc="tempdir")
```

Instead of passing the password as argument, one can alternatively provide a file containing the
password on the first line or set up SSH for passwordless login.
More information about the function `initInteractiveSSH` is available by typing
`?initInteractiveSSH` at the R prompt.

Once the connection is established, the object `sshcon` contains the function `execBash`,
which can be used to submit commands to the remote machine:

```
sshcon$execBash("echo Hello World!")
# Output
# [[1]]
# [1] "Hello World!"
```

Several commands can be executed at once:

```
sshcon$execBash(c("echo Hello World!", "echo Two commands in sequence, wow!"))
# Output
# [[1]]
# [1] "Hello World!"
# 
# [[2]]
# [1] "Two commands in sequence, wow!"
```

After the SSH connection is not longer required,
it should be closed via

```
sshcon$closeCon()
```




