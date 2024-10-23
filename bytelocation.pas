program bytelocation;
 var l:longint;
    k:word;
      bl:array[0..3] of byte absolute l;


begin
    writeln( sizeof(pointer),' ',sizeof(longint),' ',sizeof(word));



       l := $01020304;
    write(' $01020304=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;
    
       l := $04;
    write(' $04=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;

       l := $0400;
    write(' $0400=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;

       l := $040000;
    write(' $040000=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;

    
       l := $04000000;
    write(' $04000000=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;



       l := $0d0c0b0a ;
    write('$0d0c0b0a=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;

       l := 255;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;




       l := 0;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;


       l := 2;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;


       l := 256;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;
           l := $010000;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;
               l := $01000000;
    write('l=',l,' internally it''s ');
    for k:= 0 to 3 do write(bl[k],' ');
    writeln;
      l :=ORD(' ')*256*65536;
    write('Byte1Space=' ,l,' ');
    for k:= 0 to 3 do write(bl[k],' ');


      writeln;
    readln;
end.

