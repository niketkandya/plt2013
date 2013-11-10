#Testing

This is where we will put our testing scripts and source files, as well as some
notes on how we will do the testing.


## 2 Methods of testing
1. Eddie's pi
2. Your local qemu pi


### Config instructions
Put in your ~/.ssh/config file:

    Host CONFIGNAME 
    Hostname HOSTNAME
    User pi
    Port PORTNUM (not needed if port is the default 22)

Add your public key to ~/.ssh/authorized_keys on the remote machine (or
    your qemu)

### Using Eddie's pi
Follow config instructions

CONFIGNAME is edpi

HOSTNAME is the hostname we all know

### Testing with QEMU
I'm running qemu with

    qemu-system-arm -kernel kernel-qemu -cpu arm1176 -m 256 -M versatilepb
    -no-reboot -serial stdio -append "root=/dev/sda2 panic=1" -hda
    2013-09-25-wheezy-raspbian.img -redir tcp:5022::22

Follow config instructions

CONFIGNAME is qemupi

HOSTNAME is localhost

PORTNUM is 5022

## Running the tests

Run "make test_edpi" or "make test_qemupi". "make test" is only used on a pi
machine.


### Troubleshooting
It is possible that you may run into git conflicts when running either of those
make tasks (as it calls git pull). If so, just manually fix it and try again.
(Let's hope not)


### Future plans?
I'm thinking it would be helpful to just run one specific test instead of doing
them all.
