unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, Menus, dDatenbank, DB, LyricsFetcher;

type
  TTracks = class(TForm)
    dbgSongs: TDBGrid;
    imgSongCover: TDBImage;
    dbMemoSongInfo: TDBMemo;
    dbMemoLyrics: TDBMemo;
    navSongs: TDBNavigator;
    miFetchLyrics: TMenuItem;
    pmSongs: TPopupMenu;
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

{ TTracks }
// Fetch Lyrics Menu Item on Click
procedure TTracks.miFetchLyricsClick(Sender: TObject);
begin
  HandleSongClick;
end;

//used before reading/writing data to avoid errors
function TTracks.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain) and dmMain.qSongs.Active and not dmMain.qSongs.IsEmpty;
end;

//Shows current song data & Handles missing/corrupt imgs
procedure TTracks.DisplayCurrentSong;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  // lyrics, available?
  Field := dmMain.qSongs.FindField('Lyrics');
  if Assigned(Field) and (Trim(Field.AsString) <> '') then
    dbMemoLyrics.Text := Field.AsString
  else
    dbMemoLyrics.Clear;

  // song cover
  imgSongCover.Picture := nil;
  Field := dmMain.qSongs.FindField('SongCover');
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
                    dmMain.qSongs.FieldByName('ID').AsInteger, ': ', E.Message);
            imgSongCover.Picture := nil;
          end;
        end;
      end;
    finally
      BlobStream.Free;
    end;
  end;
end;

//Loads songs from Album into qSongs dataset & display in grid
procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  if not Assigned(dmMain) then Exit;
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  dmMain.qSongs.Close;
  dmMain.qSongs.ParamByName('AlbumID').AsInteger := AlbumID;    //placeholder
  dmMain.qSongs.Open;
end;

//Handles Column Click
procedure TTracks.colClick(Column: TColumn);
begin
  if CanEditDataset and (Column.FieldName = 'SongTitle') then
    HandleSongClick;
end;

//Displayes song data & lyrics. Lyrics's empty? Fetch from API
procedure TTracks.HandleSongClick;
var
  SongID: Integer;
  SongTitle, Artist, Lyrics: string;
begin
  if not CanEditDataset then Exit;

  SongID := dmMain.qSongs.FieldByName('ID').AsInteger;
  SongTitle := dmMain.qSongs.FieldByName('SongTitle').AsString;
  Artist := dmMain.qSongs.FieldByName('Artist').AsString;

  Lyrics := '';
  if Assigned(dmMain.qSongs.FindField('Lyrics')) then
    Lyrics := Trim(dmMain.qSongs.FieldByName('Lyrics').AsString);

  if Lyrics <> '' then
    dbMemoLyrics.Text := Lyrics
  else
  begin
    // synchronous fetch (UI will block while fetching)
    dbMemoLyrics.Text := 'Fetching lyrics...';
    Application.ProcessMessages; // refresh UI to show fetch msg

    Lyrics := TLyricsFetcher.GetLyricsFromAPI(SongID, SongTitle, Artist);  //builds url, sends http req, reads json, extracts lyrics then returns a tring.

    if Lyrics <> '' then
    begin
      // Save lyrics in dataset
      try
        if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
          dmMain.qSongs.Edit;
        dmMain.qSongs.FieldByName('Lyrics').AsString := Lyrics;
        dmMain.qSongs.Post;
      except
        on E: Exception do
          ShowMessage('Error saving lyrics: ' + E.Message);
      end;
      dbMemoLyrics.Text := Lyrics;
    end
    else
    begin
      dbMemoLyrics.Text := 'No lyrics found or an error occurred.';   //fallback msg
    end;
  end;
end;

end.



