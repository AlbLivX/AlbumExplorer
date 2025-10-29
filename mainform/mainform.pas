unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, RTTICtrls, dDatenbank, SongsFormUnit, DB, Uni;

type
  { TAlbums }

  TAlbums = class(TForm)
    btnConnect: TButton;
    btnLoadImage: TButton;
    col: TDBGrid;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    Img: TImage;
    OpenDialog1: TOpenDialog;
    procedure btnConnectClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure colCellClick(Column: TColumn);
  private
    procedure DisplayCurrentRecord(DataSet: TDataSet = nil);
    procedure HandleAlbumClick(AlbumID: Integer);
    function CanEditDataset: Boolean;
  public
  end;

const
  FIELD_ALBUM_COVER = 'AlbumCover';
  FIELD_DESCRIPTION = 'Description';
  FIELD_ID = 'ID';
  FIELD_ALBUM = 'ALBUM';

var
  Albums: TAlbums;

implementation

{$R *.lfm}

uses
  Variants;

{ Simple check for dataset availability }
function TAlbums.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
         and dmMain.qAdressen.Active
         and not dmMain.qAdressen.IsEmpty;
end;

{ Displays current album record’s text and image }
procedure TAlbums.DisplayCurrentRecord(DataSet: TDataSet);
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  DBMemo1.Text := dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION).AsString;
  Img.Picture := nil;

  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  if not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      Img.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

{ Connects to database and loads albums dataset }
procedure TAlbums.btnConnectClick(Sender: TObject);
begin
  ShowMessage('Connect button clicked!');
  if not Assigned(dmMain) then
  begin
    ShowMessage('dmMain is not assigned!');
    Exit;
  end;

  try
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAdressen.Active then
      dmMain.qAdressen.Open;

    DisplayCurrentRecord;

    dmMain.qAdressen.AfterScroll := @DisplayCurrentRecord;
    col.OnCellClick := @colCellClick;

     ShowMessage('Database and dataset connected!');

  except
    on E: Exception do
      ShowMessage('Database error: ' + E.Message);
  end;
end;

{ Called when clicking a cell in the album list }
procedure TAlbums.colCellClick(Column: TColumn);
begin
  if (Column.FieldName = FIELD_ALBUM) and CanEditDataset then
    HandleAlbumClick(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
end;

{ Handles what happens when an album is clicked }
procedure TAlbums.HandleAlbumClick(AlbumID: Integer);
begin
  DisplayCurrentRecord;

  case MessageDlg('Edit album (Yes) or view tracks (No)?',
                  mtConfirmation, [mbYes, mbNo], 0) of
    mrYes:
      begin
        dmMain.qAdressen.Edit;
        // optional: editing album ID AlbumID
      end;
    mrNo:
      begin
        if not Assigned(Tracks) then
          Application.CreateForm(TTracks, Tracks);

        Tracks.LoadSongsFromAlbum(AlbumID);
        Tracks.Show;
        // optional: viewing tracks for album ID AlbumID
      end;
  end;
end;

{ Loads and saves an image to the album record }
procedure TAlbums.btnLoadImageClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  if not CanEditDataset then Exit;
  if not OpenDialog1.Execute then Exit;

  try
    Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
    if not Assigned(Field) or (Field.DataType <> ftBlob) then
      raise Exception.Create('AlbumCover field missing or not a BLOB');

    dmMain.qAdressen.Edit;

    FileStream := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    try
      BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmWrite);
      try
        BlobStream.CopyFrom(FileStream, FileStream.Size);
      finally
        BlobStream.Free;
      end;
    finally
      FileStream.Free;
    end;

    dmMain.qAdressen.Post;
    DisplayCurrentRecord;
    ShowMessage('Image saved successfully!');
    // optional: image saved for current album
  except
    on E: Exception do
      ShowMessage('Error saving image: ' + E.Message);
  end;
end;

end.

