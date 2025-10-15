unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids, dDatenbank, DB;

type
  TTracks = class(TForm)
    DBGrid1: TDBGrid;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
    procedure LoadSongsFromAlbum(AlbumID: Integer);
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{ TTracks }

procedure TTracks.FormCreate(Sender: TObject);
begin

  if Assigned(dmMain) then
  begin
    DBGrid1.DataSource := dmMain.sqSongs;
    DBNavigator1.DataSource := dmMain.sqSongs;
    DBMemo1.DataSource := dmMain.sqSongs;
    DBImage1.DataSource := dmMain.sqSongs;
  end;
end;

procedure TTracks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Tracks := nil;
end;

procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  if not Assigned(dmMain) then Exit;

  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  dmMain.qSongs.Close;
  dmMain.qSongs.SQL.Text :=
    'SELECT ' +
    '  s.ID, ' +
    '  s.SongTitle, ' +
    '  s.Duration, ' +
    '  s.AlbumID, ' +
    '  a.Album, ' +
    '  a.Artist, ' +
    '  a.ReleaseYear, ' +
    '  a.Description, ' +
    '  a.AlbumCover ' +
    'FROM Songs s ' +
    'JOIN Album a ON s.AlbumID = a.ID ' +
    'WHERE s.AlbumID = :AlbumID';

  dmMain.qSongs.ParamByName('AlbumID').AsInteger := AlbumID;
  dmMain.qSongs.Open;
end;


end.

