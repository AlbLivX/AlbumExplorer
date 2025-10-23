program Album;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cthreads,{$ENDIF}
  Interfaces, Forms, Classes, runtimetypeinfocontrols,
  MainForm, dDatenbank, unidac10, ibprovider10,
  SongsFormUnit;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TTracks, Tracks);

  Application.Run;
end.


