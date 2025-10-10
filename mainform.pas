unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, dDatenbank, DB;

type
  { TForm1 }
  TForm1 = class(TForm)
    btnConnect:     TButton;
    btnLoadImage:   TButton;
    col: TDBGrid;
    dbeID:          TDBEdit;
    dbeAlbum:       TDBEdit;
    dbeArtis:       TDBEdit;
    dbeReleaseYear: TDBEdit;
    DBMemo1:       TDBMemo;
    DBNavigator1:   TDBNavigator;
    edMemo:         TMemo;
    Img:            TImage;
    Label1:         TLabel;
    OpenDialog1:    TOpenDialog;

    procedure btnConnectClick(Sender:   TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure colCellClick(Column:      TColumn);

  private
    procedure SaveMemoToDatabase;
    procedure LoadAlbumImage;
    procedure TestBlobField;
    procedure SaveImageToDatabase(const FileName: string);
    function CanEditDataset: Boolean;
    procedure SyncMemos;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  // --- Set text hints ---
  dbeID.TextHint          := 'Enter ID';
  dbeAlbum.TextHint       := 'Enter Album Name';
  dbeArtis.TextHint       := 'Enter Artist';
  dbeReleaseYear.TextHint := 'Enter Release Year';
  edMemo.TextHint         := 'Enter description';
  DBMemo1.TextHint        := 'Read-only description';

  // --- Clear visual contents safely ---
  dbeID.Text          := '';
  dbeAlbum.Text       := '';
  dbeArtis.Text       := '';
  dbeReleaseYear.Text := '';
  edMemo.Clear;
  DBMemo1.Clear;
  Img.Picture := nil;

  // --- Connect database if not connected ---
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  // --- Open dataset if not active ---
  if not dmMain.qAdressen.Active then
    dmMain.qAdressen.Open;

  // --- Assign grid click event ---
  col.OnCellClick := @colCellClick;
end;





function TForm1.CanEditDataset: Boolean;
begin
  Result := (dmMain.qAdressen.Active) and (not dmMain.qAdressen.IsEmpty);
end;

procedure TForm1.SaveMemoToDatabase;
begin
  if CanEditDataset then
  begin
    dmMain.qAdressen.Edit;
    dmMain.qAdressen.FieldByName('Description').AsString := edMemo.Text;
    dmMain.qAdressen.Post;
    SyncMemos;
  end;
end;

procedure TForm1.TestBlobField;
var
  BlobStream: TStream;
  Field: TField;
begin
  if CanEditDataset then
  begin
    Field := dmMain.qAdressen.FieldByName('AlbumCover');
    if (Field <> nil) and (Field.DataType = ftBlob) and (not Field.IsNull) then
    begin
      BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
      try
        // BLOB connection successful
      finally
        BlobStream.Free;
      end;
    end
    else
      ShowMessage('No BLOB data found or field missing.');
  end;
end;

procedure TForm1.LoadAlbumImage;
var
  BlobStream: TStream;
  Field: TField;
begin
  Img.Picture := nil;
  if CanEditDataset then
  begin
    Field := dmMain.qAdressen.FieldByName('AlbumCover');
    if (Field <> nil) and (Field.DataType = ftBlob) and (not Field.IsNull) then
    begin
      BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
      try
        Img.Picture.LoadFromStream(BlobStream);
      finally
        BlobStream.Free;
      end;
    end;
  end;
end;

procedure TForm1.btnLoadImageClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    try
      SaveImageToDatabase(OpenDialog1.FileName);
      LoadAlbumImage;
      SyncMemos;
      ShowMessage('Image saved successfully!');
    except
      on E: Exception do
        ShowMessage('Error saving image: ' + E.Message);
    end;
  end;
end;

procedure TForm1.SaveImageToDatabase(const FileName: string);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  if CanEditDataset then
  begin
    Field := dmMain.qAdressen.FieldByName('AlbumCover');
    if (Field = nil) or (Field.DataType <> ftBlob) then
      raise Exception.Create('AlbumCover field is missing or not a BLOB.');

    dmMain.qAdressen.Edit;

    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmWrite);
      try
        BlobStream.CopyFrom(FileStream, FileStream.Size);
      finally
        BlobStream.Free;
      end;
      dmMain.qAdressen.Post;
    finally
      FileStream.Free;
    end;
  end;
end;

procedure TForm1.colCellClick(Column: TColumn);
begin
  if CanEditDataset then
  begin
    // Sync both
    SyncMemos;
    // onCellClick Load Image
    LoadAlbumImage;
  end;
end;

procedure TForm1.SyncMemos;
begin
  if CanEditDataset then
  begin
    // edit Memo
    edMemo.Text := dmMain.qAdressen.FieldByName('Description').AsString;
    // read-only Memo
    DBMemo1.Refresh; // current value
  end;
end;

end.

