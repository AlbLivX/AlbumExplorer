unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DBGrids, ExtCtrls,
  DBCtrls, StdCtrls, DB, dDatenbank, SongsFormUnit;

type
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
    procedure DisplayCurrentRecord;
    function CanEditDataset: Boolean;
    procedure qAdressenAfterScroll(DataSet: TDataSet);
  public
  end;

var
  Albums: TAlbums;

implementation

{$R *.lfm}

{ TAlbums }

procedure TAlbums.DisplayCurrentRecord;
var
  Field: TField;
  BlobStream: TStream;
begin
  if not CanEditDataset then Exit;


  Field := dmMain.qAdressen.FieldByName('Description');
  if Assigned(Field) and not Field.IsNull then
    DBMemo1.Text := Field.AsString
  else
    DBMemo1.Clear;


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

function TAlbums.CanEditDataset: Boolean;
begin
  Result := (dmMain.qAdressen.Active) and (not dmMain.qAdressen.IsEmpty);
end;

procedure TAlbums.qAdressenAfterScroll(DataSet: TDataSet);
begin
  DisplayCurrentRecord;
end;

procedure TAlbums.btnConnectClick(Sender: TObject);
begin
  if not dmMain.cDatenbank.Connected then
    dmMain.cDatenbank.Connected := True;

  if not dmMain.qAdressen.Active then
    dmMain.qAdressen.Open;

  // Hook grid click handler
  col.OnCellClick := @colCellClick;

  // Hook dataset scroll handler
  dmMain.qAdressen.AfterScroll := @qAdressenAfterScroll;



end;

procedure TAlbums.colCellClick(Column: TColumn);
var
  AlbumID: Integer;
  Choice: Integer;
begin
  if Column.FieldName = 'ALBUM' then
  begin
    DisplayCurrentRecord;

    Choice :=
    MessageDlg('For Editting Album click "Yes", for Viewing Tracks Click "No" ', mtConfirmation, [mbYes, mbNo], 0);

    if Choice = mrYes then
    begin
      if not (dmMain.qAdressen.State in [dsEdit, dsInsert]) then
        dmMain.qAdressen.Edit;
    end
    else
    begin
      AlbumID := dmMain.qAdressen.FieldByName('ID').AsInteger;

      if not Assigned(Tracks) then
        Application.CreateForm(TTracks, Tracks);
      Tracks.LoadSongsFromAlbum(AlbumID);
      Tracks.Show;
    end;
  end;
end;

procedure TAlbums.btnLoadImageClick(Sender: TObject);
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

      if not (dmMain.qAdressen.State in [dsEdit, dsInsert]) then
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

      DisplayCurrentRecord;
      ShowMessage('Image saved successfully!');
    except
      on E: Exception do
        ShowMessage('Error saving image: ' + E.Message);
    end;
  end;
end;

end.

