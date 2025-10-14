unit SongsFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids, dDatenbank, DB;

type
  TTracks = class(TForm)
    DBGrid1:      TDBGrid;
    DBImage1:     TDBImage;
    DBMemo1:      TDBMemo;
    DBNavigator1: TDBNavigator;
  private
  public
procedure LoadSongsFromAlbum(AlbumID: Integer);
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{ TTracks }

procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  dmMain.qSongs.Close;
  dmMain.qSongs.SQL.Text := 'SELECT * FROM SONGS WHERE AlbumID = :AlbumID';
  dmMain.qSongs.ParamByName('AlbumID').AsInteger := AlbumID;
  dmMain.qSongs.Open;
end;

end.


