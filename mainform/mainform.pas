unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, Buttons, CheckLst, dDatenbank, SongsFormUnit,
  DB, Uni;

type
  { TAlbums }

  TAlbums = class(TForm)
    btnDBConnect: TButton;
    btnLoadAlbumCover: TButton;
    dbgAlbums: TDBGrid;
    dbMemoAlbumDescription: TDBMemo;
    navAlbums: TDBNavigator;
    imgAlbumCover: TImage;
    OpenDialog1: TOpenDialog;
    procedure btnDBConnectClick(Sender: TObject);
    procedure btnLoadAlbumCoverClick(Sender: TObject);
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

//dataset available?
function TAlbums.CanEditDataset: Boolean;
begin
  Result := Assigned(dmMain)
         and dmMain.qAdressen.Active
         and not dmMain.qAdressen.IsEmpty;
end;

//Description and Album Cover
procedure TAlbums.DisplayCurrentRecord(DataSet: TDataSet);
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  dbMemoAlbumDescription.Text := dmMain.qAdressen.FieldByName(FIELD_DESCRIPTION).AsString;
  imgAlbumCover.Picture := nil;

  Field := dmMain.qAdressen.FieldByName(FIELD_ALBUM_COVER);
  if not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      imgAlbumCover.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

//Connects to db and loads albums ds
procedure TAlbums.btnDBConnectClick(Sender: TObject);
begin
  writeln('Connect button clicked!');

  if not Assigned(dmMain) then
  begin
    writeln('dmMain is not assigned!');
    Exit;
  end;

  try
    if not dmMain.cDatenbank.Connected then
      dmMain.cDatenbank.Connected := True;

    if not dmMain.qAdressen.Active then
      dmMain.qAdressen.Open;

    writeln('Dataset opened. Record count: ', dmMain.qAdressen.RecordCount);

    DisplayCurrentRecord;

    dmMain.qAdressen.AfterScroll := @DisplayCurrentRecord;
    dbgAlbums.OnCellClick := @colCellClick;

  except
    on E: Exception do
      writeln('Database error: ', E.Message);
  end;
end;


//Called on cell click in the table
procedure TAlbums.colCellClick(Column: TColumn);
begin
  if (Column.FieldName = FIELD_ALBUM) and CanEditDataset then
    HandleAlbumClick(dmMain.qAdressen.FieldByName(FIELD_ID).AsInteger);
end;


//on an Album Click Handler
procedure TAlbums.HandleAlbumClick(AlbumID: Integer);
begin
  DisplayCurrentRecord;

  case MessageDlg('Edit album (Yes) or view tracks (No)?',
                  mtConfirmation, [mbYes, mbNo], 0) of
    mrYes:
      begin
        dmMain.qAdressen.Edit;
      end;
    mrNo:
      begin
        if not Assigned(Tracks) then
          Application.CreateForm(TTracks, Tracks);

        Tracks.LoadSongsFromAlbum(AlbumID);
        Tracks.Show;
      end;
  end;
end;

//loads +/ saves imgs  to Album Record
procedure TAlbums.btnLoadAlbumCoverClick(Sender: TObject);
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
  except
    on E: Exception do
      ShowMessage('Error saving image: ' + E.Message);
  end;
end;

end.

