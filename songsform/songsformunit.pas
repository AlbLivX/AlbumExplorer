unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, Menus, dDatenbank, DB, LyricsFetcher, uConstants,  Uni;

type
  { TTracks: Form for managing songs of an album }

  TTracks = class(TForm)
    dbgSongs:       TDBGrid;
    imgSongCover:   TDBImage;
    dbMemoSongInfo: TDBMemo;
    dbMemoLyrics:   TDBMemo;
    navSongs:       TDBNavigator;
    miFetchLyrics:  TMenuItem;
    pmSongs:        TPopupMenu;

    procedure miFetchLyricsClick(Sender: TObject);
    procedure dbgSongsCellClick(Column: TColumn);

  private
    { Checks if songs dataset is available and editable }
    function CanEditDataset: Boolean;

    { Displays currently selected song's details including lyrics }
    procedure DisplayCurrentSong;

  public
    { Loads all songs from a given album ID }
    procedure LoadSongsFromAlbum(AlbumID: Integer);

    { Handles song click: fetches lyrics if not available and displays details }
    procedure HandleSongClick;
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{ Menu Action }

procedure TTracks.miFetchLyricsClick(Sender: TObject);
begin
  // Fetch lyrics for selected song
  HandleSongClick;
end;

{ Utility }

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

  // Display lyrics if available
  Field := dmMain.qSongs.FindField(FIELD_LYRICS);
  if Assigned(Field) and (Trim(Field.AsString) <> '') then
    dbMemoLyrics.Text := Field.AsString
  else
    dbMemoLyrics.Clear;

  // Load song cover image if available
  imgSongCover.Picture := nil;
  Field := dmMain.qSongs.FindField(FIELD_ALBUM_COVER);
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qSongs.CreateBlobStream(Field, bmRead);
    try
      if BlobStream.Size > 0 then
        imgSongCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

{ DB Actions }

procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  // Loads songs from a given album into the songs dataset
  if not Assigned(dmMain) then Exit;
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  dmMain.qSongs.Close;
  dmMain.qSongs.ParamByName(PARAM_ALBUM_ID).AsInteger := AlbumID;
  dmMain.qSongs.Open;
end;

{ Grid Click }

procedure TTracks.dbgSongsCellClick(Column: TColumn);
begin
  // Handles song grid click to display lyrics and details
  if CanEditDataset and (Column.FieldName = FIELD_SONG_TITLE) then
    HandleSongClick;
end;

{ Main Logic }

procedure TTracks.HandleSongClick;
var
  SongID: Integer;
  SongTitle, Artist, Lyrics: string;
begin
  // Fetch and display lyrics for the selected song
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


