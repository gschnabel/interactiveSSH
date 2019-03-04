# interactiveSSH package

This R package provides functionality to login over SSH
to a remote machine, execute commands on it 
and capture the output.

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
is done like that: 

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

If the SSH connection is not longer required,
it should be closed via

```
sshcon$closeCon()
```

## Troubleshooting

If the function `initInteractiveSSH` throws an error during a connection attempt,
the following steps may help to locate the problem.

In some scenarios, `initInteractiveSSH` throws an error including the SSH command attempted to run
in the error message.
Running this command manually in a terminal may give a hint about the problem.

After the connection has been established, the package tries to alter the bash prompt 
in order to apply regular expressions to capture the output of commands.
If changing the prompt fails, e.g., because SSH executes another shell than bash,
the problem may be investigated by passing `PS1=NULL` as argument to 
`initInteractiveSSH`. The function will then not try to change the prompt but returns
immediately as soon as the SSH connection is established.
The function calls `send("<command>")` and `read()` can then be used to 
diagnose the problem. Read more about these function by typing `?initInteractiveSSH`
at the R prompt.
