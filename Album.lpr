program Album;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, unidac10, LyricsFetcher, dDatenbank, MainForm,
  SongsFormUnit, DB, uConstants, LoginFormUnit, RegisterFormUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
  {$POP}
  Application.Initialize;
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TTracks, Tracks);
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TLogin, LoginForm);
  Application.CreateForm(TRegister, RegisterForm);
  Application.Run;
end.

