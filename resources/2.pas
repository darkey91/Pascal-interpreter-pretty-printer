PROGRAM Sort;

VAR
    i, j, tmp, size: integer;

function wrongSum(a, b : integer): integer;
var i, j, tmp, size: integer;
BEGIN
            writeLn(a + b);
            wrongSum := a + b;
END;

PROCEDURE ReadArr;
BEGIN

	readln(size);
      for i := 1 to size do
        writeLn('Hi');

END;

BEGIN
    ReadArr;

  writeln('Size: ', size);
    FOR i := 1 TO size DO
    for j := 2 TO size DO
        begin
        writeLn('not yet');
        tmp := wrongSum(42, 16);
        writeLn('can');
        end;
    Writeln('Wrong sum = ', tmp);
END.