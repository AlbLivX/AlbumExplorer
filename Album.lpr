program Album;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, LCLType, unidac10, LyricsFetcher, dDatenbank,
  MainForm, SongsFormUnit, DB, uConstants, LoginFormUnit, RegisterFormUnit;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TLogin, LoginForm);
  Application.CreateForm(TRegister, RegisterForm);
  Application.CreateForm(TTracks, Tracks);


  LoginForm.ShowModal;


  if LoginForm.LoginSuccessful then
  begin
    Application.Run;
  end
  else
    Application.Terminate;
end.
