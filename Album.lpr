program Album;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, LCLType, DB,
  dDatenbank, Uni, ibprovider10 , LoginFormUnit, MainForm;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TLoginForm, LoginForm);






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

