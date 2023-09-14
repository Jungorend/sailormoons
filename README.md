# Sailor Moon S Disassembly

Attempt to document the SMS codebase to understand its inner workings better. The goal is to have a full transition from machine disassembly to documented code.

The disassembly here is based on the Big Zam Edition presently.

I wanted to keep this separate from changes/updates, so it does not include the changes made in the Tournament Edition, sometimes called the Jungy Edition (Removes Sailor Uranus, who is generally banned in competitive play).


## Usage
This is written for usage with asar, a 65c816 assembler. 

Drag main.asm onto asar to compile into the full sfc file. 

## For New Viewers:
The majority of the documented code is presently in Bank_C0. This seems to also be where the majority of the code is written. Functions where their purpose has been discovered, have been named.

Pull Requests are welcome.
