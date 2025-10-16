unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, dDatenbank, DB,
  fphttpclient, fpjson, jsonparser, HTTPDefs;

type

  { TTracks }

  TTracks = class(TForm)
    DBGrid1: TDBGrid;
    DBImage1: TDBImage;
    DBMemo1: TDBMemo;
    DBMemo2: TDBMemo;
    DBNavigator1: TDBNavigator;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    function FetchLyricsFromFabrix(const Artist, SongTitle: string): string;
  public
    procedure LoadSongsFromAlbum(AlbumID: Integer);
    procedure colClick(Column: TColumn);
    function CanEditDataset: Boolean;
    procedure HandleSongClick;
    procedure DisplayCurrentSong;
    procedure ShowLyricsFromFabrix(SongTitle, Artist: string);
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{ TTracks }

function TTracks.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain) and dmMain.qSongs.Active and not dmMain.qSongs.IsEmpty;
end;

procedure TTracks.DisplayCurrentSong;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  // Display lyrics from DB if present
  Field := dmMain.qSongs.FieldByName('Lyrics');
  if Assigned(Field) and not Field.IsNull then
    DBMemo2.Text := Field.AsString
  else
    DBMemo2.Clear;

  // Display song artwork if available
  Field := dmMain.qSongs.FieldByName('SongCover');
  DBImage1.Picture := nil;
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qSongs.CreateBlobStream(Field, bmRead);
    try
      DBImage1.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

function TTracks.FetchLyricsFromFabrix(const Artist, SongTitle: string): string;
var
  Client: TFPHTTPClient;
  Response: TStringStream;
  JSON: TJSONData;
  LyricsArray: TJSONArray;
  I: Integer;
begin
  Result := '';
  Client := TFPHTTPClient.Create(nil);
  Response := TStringStream.Create('');
  try
    // Replace YOUR_API_KEY_HERE with your Fabrix API key
    Client.AddHeader('x-api-key', 'YOUR_API_KEY_HERE');
    Client.Get(
      'https://api.openapihub.com/song-lyrics/lyrics?song_title=' +
      EncodeURLElement(SongTitle) + '&artist_name=' +
      EncodeURLElement(Artist),
      Response
    );
    ShowMessage(Response.DataString);


    JSON := GetJSON(Response.DataString);
    if JSON.FindPath('lyrics') <> nil then
    begin
      LyricsArray := TJSONArray(JSON.FindPath('lyrics'));
      for I := 0 to LyricsArray.Count - 1 do
        Result := Result + LyricsArray.Strings[I] + LineEnding;
    end;
    ShowMessage(Response.DataString);

  finally
    Response.Free;
    Client.Free;
  end;
end;

procedure TTracks.ShowLyricsFromFabrix(SongTitle, Artist: string);
var
  Lyrics: string;
begin
  Lyrics := FetchLyricsFromFabrix(Artist, SongTitle);
  DBMemo2.Lines.Text := Lyrics;

  // Optionally save lyrics to DB
  if CanEditDataset then
  begin
    if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
      dmMain.qSongs.Edit;
    dmMain.qSongs.FieldByName('Lyrics').AsString := Lyrics;
    dmMain.qSongs.Post;
  end;
end;

procedure TTracks.HandleSongClick;
var
  Choice: Integer;
  SongTitle, Artist: string;
begin
  if not CanEditDataset then Exit;

  SongTitle := dmMain.qSongs.FieldByName('SongTitle').AsString;
  Artist := dmMain.qSongs.FieldByName('Artist').AsString;

  Choice := MessageDlg(
    'For editing the song click "Yes", for viewing lyrics click "No".',
    mtConfirmation, [mbYes, mbNo], 0
  );

  if Choice = mrYes then
  begin
    if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
      dmMain.qSongs.Edit;
    DBMemo2.ReadOnly := False;
  end
  else
  begin
    // Show DB lyrics if present; otherwise fetch from API
    if (dmMain.qSongs.FieldByName('Lyrics').IsNull) or
       (dmMain.qSongs.FieldByName('Lyrics').AsString = '') then
    begin
      ShowLyricsFromFabrix(SongTitle, Artist);
    end
    else
      DBMemo2.Text := dmMain.qSongs.FieldByName('Lyrics').AsString;

    DBMemo2.ReadOnly := True;
  end;
end;

procedure TTracks.colClick(Column: TColumn);
begin
  if (Column.FieldName = 'SongTitle') and CanEditDataset then
    HandleSongClick;
end;

procedure TTracks.FormCreate(Sender: TObject);
begin
  if Assigned(dmMain) then
  begin
    DBGrid1.DataSource := dmMain.sqSongs;
    DBNavigator1.DataSource := dmMain.sqSongs;
    DBMemo1.DataSource := dmMain.sqSongs;
    DBImage1.DataSource := dmMain.sqSongs;
    DBMemo2.ReadOnly := True;
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
  dmMain.qSongs.ParamByName('AlbumID').AsInteger := AlbumID;
  dmMain.qSongs.Open;
end;

end.
