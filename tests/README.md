#Testing

This is where we will put our testing scripts and source files, as well as some
notes on how we will do the testing.


### Testing with QEMU
I'm just going to document what I'm doing. Feel free to add whatever you want.

If you run qemu with the flag "-redir tcp:5022::22", then you can ssh into qemu
with

    ssh pi@locahost -p 5022


On your local machine, add to ~/.ssh/config

    Host pi
    Hostname localhost
    Port 5022
    User pi

So now you can ssh into qemu with "ssh pi"

For password-less login, add your public key to ~/.ssh/authorized_hosts in the
qemu vm


####Automatic testing
Assuming you have the configurations above and you are in the directory where
the assembly files are, run:

    rsync -avz -e ssh . pi:tmp/ # Does all work in a ~/tmp directory
    ssh pi "cd tmp; make run"   # Assuming you have a "run" task for Makefile
                                # that compiles and runs the assembly

(Maybe we can standardize this in the future?)
(Maybe we can create a make task that does all of this?)
Any other ideas?
