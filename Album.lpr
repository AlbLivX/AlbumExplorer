program Album;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, Forms, runtimetypeinfocontrols,
  MainForm, dDatenbank, unidac10, ibprovider10,
  SongsFormUnit, AlbumExplorerAPI, uAPI, fphttpapp;

{$R *.res}

type
  TServerThread = class(TThread)
  private
    FServer: TFPHTTPServer;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  ServerThread: TServerThread;

constructor TServerThread.Create;
begin
  inherited Create(False); // start immediately
  FServer := TFPHTTPServer.Create(nil);
  FServer.Port := 8080;
  FServer.OnRequest := @OnRequestHandler;
end;

destructor TServerThread.Destroy;
begin
  FServer.Free;
  inherited;
end;

procedure TServerThread.Execute;
begin
  FServer.Active := True;  // server runs in background
end;

begin
  RequireDerivedFormResource := True;

  Application.Initialize;
  Application.CreateForm(TAlbums, Albums);
  Application.CreateForm(TdmMain, dmMain);
  Application.CreateForm(TTracks, Tracks);

  // start HTTP server thread
  ServerThread := TServerThread.Create;

  Application.Run;

  // free server after app closes
  ServerThread.Free;
end.

