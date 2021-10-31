# uzura1_fpm
uzura1 for fpm (Fortran package manager) ver 0.4

Fortran Package Manager is required.
https://github.com/fortran-lang/fpm

$cd uzura1_fpm

$fpm build --profile release

$fpm run

```
 Usage : uzura1 -option file_name
       : file_name.wav -> file_name.mp1
 Option: -b 1..14  bitrate (default 12: 384kbps)
         -crc      CRC16 error protection on
         -c        copyright flag on
         -o        original  flag on
```
## encode fn.wav file to fn.mp1 file

$fpm run -- -b 10 fn


# bug fix etc

- 4 space indent

- name decoration for module/type  changed to postfix from prefix 

- monoral file treated properly now

- fix for maxloc function behavior changed in f03 from f95 (intel fortran)  
