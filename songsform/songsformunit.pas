unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, Menus, dDatenbank, DB, LyricsFetcher, uConstants;

type
  TTracks = class(TForm)
    dbgSongs:       TDBGrid;
    imgSongCover:   TDBImage;
    dbMemoSongInfo: TDBMemo;
    dbMemoLyrics:   TDBMemo;
    navSongs:       TDBNavigator;
    miFetchLyrics:  TMenuItem;
    pmSongs:        TPopupMenu;

    procedure miFetchLyricsClick(Sender: TObject);

  private
    function CanEditDataset: Boolean;
    procedure DisplayCurrentSong;
    procedure colClick(Column: TColumn);

  public
    procedure LoadSongsFromAlbum(AlbumID: Integer);
    procedure HandleSongClick;
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{===========================}
{ Menu Actions              }
{===========================}

procedure TTracks.miFetchLyricsClick(Sender: TObject);
begin
  HandleSongClick;
end;

{===========================}
{ Dataset Utilities         }
{===========================}


function TTracks.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
        and Assigned(dmMain.qSongs)
        and dmMain.qSongs.Active
        and not dmMain.qSongs.IsEmpty;
end;


procedure TTracks.DisplayCurrentSong;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  Field := dmMain.qSongs.FindField(FIELD_LYRICS);
  if Assigned(Field) and (Trim(Field.AsString) <> '') then
    dbMemoLyrics.Text := Field.AsString
  else
    dbMemoLyrics.Clear;

  imgSongCover.Picture := nil;
  Field := dmMain.qSongs.FindField(FIELD_ALBUM_COVER);
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qSongs.CreateBlobStream(Field, bmRead);
    try
      if BlobStream.Size > 0 then
      begin
        try
          imgSongCover.Picture.LoadFromStream(BlobStream);
        except
          on E: Exception do
          begin
            writeln('Could not load song cover for song ID ',
                    dmMain.qSongs.FieldByName(FIELD_SONG_ID).AsInteger, ': ', E.Message);
            imgSongCover.Picture := nil;
          end;
        end;
      end;
    finally
      BlobStream.Free;
    end;
  end;
end;

{===========================}
{ Database Actions          }
{===========================}


procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  if not Assigned(dmMain) then Exit;
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  dmMain.qSongs.Close;
  dmMain.qSongs.ParamByName(PARAM_ALBUM_ID).AsInteger := AlbumID;
  dmMain.qSongs.Open;
end;

{===========================}
{ Grid Handlers             }
{===========================}


procedure TTracks.colClick(Column: TColumn);
begin
  if CanEditDataset and (Column.FieldName = FIELD_SONG_TITLE) then
    HandleSongClick;
end;

{===========================}
{ Song Actions             }
{===========================}


procedure TTracks.HandleSongClick;
var
  SongID: Integer;
  SongTitle, Artist, Lyrics: string;
begin
  if not CanEditDataset then Exit;

  SongID := dmMain.qSongs.FieldByName(FIELD_SONG_ID).AsInteger;
  SongTitle := dmMain.qSongs.FieldByName(FIELD_SONG_TITLE).AsString;
  Artist := dmMain.qSongs.FieldByName(FIELD_ARTIST).AsString;

  Lyrics := '';
  if Assigned(dmMain.qSongs.FindField(FIELD_LYRICS)) then
    Lyrics := Trim(dmMain.qSongs.FieldByName(FIELD_LYRICS).AsString);

  if Lyrics <> '' then
    dbMemoLyrics.Text := Lyrics
  else
  begin
    dbMemoLyrics.Text := 'Fetching lyrics...';
    Application.ProcessMessages;

    Lyrics := TLyricsFetcher.GetLyricsFromAPI(SongID, SongTitle, Artist);

    if Lyrics <> '' then
    begin
      try
        if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
          dmMain.qSongs.Edit;
        dmMain.qSongs.FieldByName(FIELD_LYRICS).AsString := Lyrics;
        dmMain.qSongs.Post;
      except
        on E: Exception do
          ShowMessage('Error saving lyrics: ' + E.Message);
      end;
      dbMemoLyrics.Text := Lyrics;
    end
    else
      dbMemoLyrics.Text := 'No lyrics found or an error occurred.';
  end;
end;

end.

