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
      p.options.add('-Sc');
      p.SourcePath.Add('src');
      p.targets.AddProgram('aoc24.pas');
      p.targets.AddUnit('aoc.pas');
      p.targets.AddUnit('aoc.generic.pas');
      p.targets.AddUnit('day01.pas');
      run;
   end
end.
