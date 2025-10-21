unit SongsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBCtrls, DBGrids,
  StdCtrls, ExtCtrls, dDatenbank, DB;

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
    function CanEditDataset: Boolean;
    procedure DisplayCurrentSong;
  public
    procedure LoadSongsFromAlbum(AlbumID: Integer);
    procedure colClick(Column: TColumn);
    procedure HandleSongClick;
  end;

var
  Tracks: TTracks;

implementation

{$R *.lfm}

function TTracks.CanEditDataset: Boolean;
begin
  //ShowMessage('Checking if dataset can be edited');
  Result := Assigned(dmMain) and dmMain.qSongs.Active and not dmMain.qSongs.IsEmpty;
end;

procedure TTracks.DisplayCurrentSong;
var
  Field: TField;
  BlobStream: TStream;
begin
  //ShowMessage('Displaying current song');
  if not CanEditDataset then Exit;

  Field := dmMain.qSongs.FieldByName('Lyrics');
  if Assigned(Field) and not Field.IsNull then
    DBMemo2.Text := Field.AsString
  else
    DBMemo2.Clear;

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

procedure TTracks.HandleSongClick;
var
  Choice: Integer;
begin
  //ShowMessage('Handling song click');
  if not CanEditDataset then Exit;

  Choice := MessageDlg(
    'For editing the song click "Yes", for viewing lyrics click "No".',
    mtConfirmation, [mbYes, mbNo], 0
  );

  if Choice = mrYes then
  begin
    //ShowMessage('Editing song');
    if not (dmMain.qSongs.State in [dsEdit, dsInsert]) then
      dmMain.qSongs.Edit;
    DBMemo2.ReadOnly := False;
  end
  else
  begin
    //ShowMessage('Viewing lyrics');
    DBMemo2.Text := dmMain.qSongs.FieldByName('Lyrics').AsString;
    DBMemo2.ReadOnly := True;
  end;
end;

procedure TTracks.colClick(Column: TColumn);
begin
  //ShowMessage('Column clicked: ' + Column.FieldName);
  if (Column.FieldName = 'SongTitle') and CanEditDataset then
    HandleSongClick;
end;

procedure TTracks.FormCreate(Sender: TObject);
begin
  //ShowMessage('FormCreate called');
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
  //ShowMessage('FormClose called');
  CloseAction := caFree;
  Tracks := nil;
end;

procedure TTracks.LoadSongsFromAlbum(AlbumID: Integer);
begin
  //ShowMessage('Loading songs for album ID: ' + IntToStr(AlbumID));
  if not Assigned(dmMain) then Exit;

  if not dmMain.cDatenbank.Connected then
  begin
    dmMain.cDatenbank.Connected := True;
    //ShowMessage('Database connected');
  end;

  dmMain.qSongs.Close;
  dmMain.qSongs.ParamByName('AlbumID').AsInteger := AlbumID;
  dmMain.qSongs.Open;
  //ShowMessage('Songs dataset opened');
end;
end.

