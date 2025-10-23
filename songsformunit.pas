unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, dDatenbank, DB, fphttpclient, fpjson, jsonparser;

type

  { TLyricsThread }

  TLyricsThread = class(TThread)
  private
    FSongID: Integer;
    FLyrics: string;
  protected
    procedure Execute; override;
  public
    constructor Create(SongID: Integer);
    property Lyrics: string read FLyrics;
  end;

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
    function CanEditDataset: Boolean;
    procedure DisplayCurrentSong;
    procedure FetchLyricsAsync(SongID: Integer);
    procedure LyricsThreadDone(Sender: TObject);
  public
    procedure LoadSongsFromAlbum(AlbumID: Integer);
    procedure colClick(Column: TColumn);
    procedure HandleSongClick;
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

{ TLyricsThread }

constructor TLyricsThread.Create(SongID: Integer);
begin
  inherited Create(False); // Start immediately
  FreeOnTerminate := True;
  FSongID := SongID;
end;

procedure TLyricsThread.Execute;
var
  URL, Response: string;
  JSONObject: TJSONObject;
begin
  // Fetch lyrics from API
  URL := Format('http://localhost:30000/lyrics/%d', [FSongID]);
  try
    Response := TFPHTTPClient.SimpleGet(URL);
    JSONObject := GetJSON(Response) as TJSONObject;
    try
      FLyrics := JSONObject.Get('lyrics', '');
    finally
      JSONObject.Free;
    end;
  except
    on E: Exception do
      FLyrics := 'Error fetching lyrics: ' + E.Message;
  end;
end;

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

  // Display lyrics from dataset if available
  Field := dmMain.qSongs.FieldByName('Lyrics');
  if Assigned(Field) and not Field.IsNull then
    DBMemo2.Text := Field.AsString
  else
    DBMemo2.Clear;

  // Display song cover
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

procedure TTracks.FetchLyricsAsync(SongID: Integer);
var
  LyricsThread: TLyricsThread;
begin
  // Create thread
  LyricsThread := TLyricsThread.Create(SongID);
  // Assign OnTerminate event to update GUI safely in main thread
  LyricsThread.OnTerminate := @LyricsThreadDone;
end;

procedure TTracks.LyricsThreadDone(Sender: TObject);
begin
  if (Sender is TLyricsThread) and CanEditDataset then
    DBMemo2.Text := TLyricsThread(Sender).Lyrics;
end;

procedure TTracks.HandleSongClick;
var
  SongID: Integer;
begin
  if not CanEditDataset then Exit;

  SongID := dmMain.qSongs.FieldByName('ID').AsInteger;

  case MessageDlg('Edit the song (Yes) or view lyrics (No)?',
                  mtConfirmation, [mbYes, mbNo], 0) of
    mrYes:
      begin
        if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
          dmMain.qSongs.Edit;
        DBMemo2.ReadOnly := False;
      end;
    mrNo:
      begin
        // Display existing lyrics if any
        DisplayCurrentSong;
        DBMemo2.ReadOnly := True;

        // Fetch lyrics asynchronously
        FetchLyricsAsync(SongID);
      end;
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

