program Album;

{$mode objfpc}{$H+}

uses
  Forms, Interfaces, LCLType, DB, dDatenbank, Uni, ibprovider10, LoginFormUnit,
  MainFormUnit, AlbumModel;

{$R *.res}

begin
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TpmAlbum, pmAlbum);
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

