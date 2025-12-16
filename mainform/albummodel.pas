unit AlbumModel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Graphics, Dialogs, ExtCtrls;

type
  { TAlbumModel manages album data, including album cover BLOBs }
  TAlbumModel = class
  private
    FDataSet: TDataSet;
  public
    constructor Create(ADataSet: TDataSet);

    function HasCover: Boolean;
    function CreateCoverStream: TStream;

    procedure LoadCoverFromDialog(TargetImage: TImage; Dialog: TOpenDialog);
  end;

implementation

{ -------------------- CONSTRUCTOR -------------------- }

constructor TAlbumModel.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

{ -------------------- COVER CHECK -------------------- }

function TAlbumModel.HasCover: Boolean;
var
  Field: TField;
begin
  Result := False;
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  Field := FDataSet.FieldByName('ALBUMCOVER');
  Result := Assigned(Field) and not Field.IsNull;
end;

{ -------------------- CREATE STREAM -------------------- }

function TAlbumModel.CreateCoverStream: TStream;
var
  Field: TField;
begin
  Result := nil;
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;

  Field := FDataSet.FieldByName('ALBUMCOVER');
  if Assigned(Field) and not Field.IsNull then
    Result := FDataSet.CreateBlobStream(Field, bmRead);
end;

{ -------------------- LOAD FROM DIALOG -------------------- }

procedure TAlbumModel.LoadCoverFromDialog(TargetImage: TImage; Dialog: TOpenDialog);
var
  FileStream, BlobStream: TStream;
  Field: TField;
begin
  if not Assigned(FDataSet) or not FDataSet.Active then Exit;
  if not Assigned(TargetImage) or not Assigned(Dialog) then Exit;
  if not Dialog.Execute then Exit;

  Field := FDataSet.FieldByName('ALBUMCOVER');
  if not Assigned(Field) then
    raise Exception.Create('ALBUMCOVER field missing');

  FDataSet.Edit;

  { Copy file into BLOB field }
  FileStream := TFileStream.Create(Dialog.FileName, fmOpenRead);
  try
    BlobStream := FDataSet.CreateBlobStream(Field, bmWrite);
    try
      BlobStream.CopyFrom(FileStream, FileStream.Size);
    finally
      BlobStream.Free;
    end;
  finally
    FileStream.Free;
  end;

  FDataSet.Post;

  { Also display in TImage immediately }
  FileStream := TFileStream.Create(Dialog.FileName, fmOpenRead);
  try
    TargetImage.Picture.LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

end.

