unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, dDatenbank, DB;

type
  { TForm1 }
  TForm1 = class(TForm)
    btnConnect: TButton;
    btnLoadImage: TButton;
    col: TDBGrid;
    dbeID: TDBEdit;
    dbeAlbum: TDBEdit;
    dbeArtis: TDBEdit;
    dbeReleaseYear: TDBEdit;
    DBMemo1: TDBMemo;
    DBNavigator1: TDBNavigator;
    edMemo: TMemo;
    Img: TImage;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;

    procedure btnConnectClick(Sender: TObject);
    procedure btnLoadImageClick(Sender: TObject);
    procedure colCellClick(Column: TColumn);
    procedure DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);

  private
    procedure ClearEditFields;
    procedure DisplayCurrentRecord;
    procedure SaveDescription;
    procedure LoadAlbumImage;
    function CanEditDataset: Boolean;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ClearEditFields;
begin
  dbeID.Text := '';
  dbeAlbum.Text := '';
  dbeArtis.Text := '';
  dbeReleaseYear.Text := '';
  DBMemo1.Clear;
  edMemo.Clear;
  Img.Picture := nil;
end;

procedure TForm1.DisplayCurrentRecord;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;

  // Show description in display memo
  Field := dmMain.qAdressen.FieldByName('Description');
  if Assigned(Field) and not Field.IsNull then
    edMemo.Text := Field.AsString
  else
    edMemo.Clear;

  // Show album image
  Field := dmMain.qAdressen.FieldByName('AlbumCover');
  Img.Picture := nil;
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      Img.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

procedure TForm1.SaveDescription;
begin
  if not CanEditDataset then Exit;

  dmMain.qAdressen.Edit;
  try
    // Save description from DBMemo1
    dmMain.qAdressen.FieldByName('Description').AsString := DBMemo1.Text;
    dmMain.qAdressen.Post;

    // Clear editing fields
    ClearEditFields;

    // Show saved description in display memo
    DisplayCurrentRecord;

    ShowMessage('Description saved successfully!');
  except
    on E: Exception do
    begin
      dmMain.qAdressen.Cancel;
      ShowMessage('Error saving description: ' + E.Message);
    end;
  end;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  ClearEditFields;

  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;
  if not dmMain.qAdressen.Active then
    dmMain.qAdressen.Open;

  col.OnCellClick := @colCellClick;
end;

procedure TForm1.colCellClick(Column: TColumn);
begin
  DisplayCurrentRecord;
end;

procedure TForm1.DBNavigator1Click(Sender: TObject; Button: TDBNavButtonType);
begin
  if Button = nbPost then
    SaveDescription;
end;

procedure TForm1.LoadAlbumImage;
var
  BlobStream: TStream;
  Field: TField;
begin
  Img.Picture := nil;
  if not CanEditDataset then Exit;

  Field := dmMain.qAdressen.FieldByName('AlbumCover');
  if Assigned(Field) and (Field.DataType = ftBlob) and not Field.IsNull then
  begin
    BlobStream := dmMain.qAdressen.CreateBlobStream(Field, bmRead);
    try
      Img.Picture.LoadFromStream(BlobStream);
    finally
      BlobStream.Free;
    end;
  end;
end;

procedure TForm1.btnLoadImageClick(Sender: TObject);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  if not CanEditDataset then Exit;

  if OpenDialog1.Execute then
  begin
    try
      Field := dmMain.qAdressen.FieldByName('AlbumCover');
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
        dmMain.qAdressen.Post;
      finally
        FileStream.Free;
      end;

      LoadAlbumImage;
      ShowMessage('Image saved successfully!');
    except
      on E: Exception do
        ShowMessage('Error saving image: ' + E.Message);
    end;
  end;
end;

function TForm1.CanEditDataset: Boolean;
begin
  Result := (dmMain.qAdressen.Active) and (not dmMain.qAdressen.IsEmpty);
end;

end.

