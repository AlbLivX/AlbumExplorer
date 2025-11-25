program Album;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, LCLType, LyricsFetcher, MainForm, SongsFormUnit, DB,
  uConstants, dDatenbank, LoginFormUnit, RegisterFormUnit, Uni, ibprovider10,
  UserObjectUnit;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TRegisterForm, RegisterForm);


  if LoginForm.Execute then
  begin
    LoginForm.Destroy;
    Application.Run;
  end
  else
  begin
    Application.Terminate;
  end;
end.

