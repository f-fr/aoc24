program fpmake;

uses fpmkunit;

var
   p: TPackage;
begin
   with Installer do begin
      p := AddPackage('aoc24');
      p.author := 'Frank Fischer';
      p.license := 'GPLv3';
      p.dependencies.add('rtl-console');
      p.dependencies.add('rtl-generics');
      p.dependencies.add('cmdargs');
      p.options.add('-Sc');
      p.targets.AddProgram('src/aoc24.pas');
      run;
   end
end.
