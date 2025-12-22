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
    function HasValidDataset: Boolean;

    function HasCover: Boolean;
    function CreateCoverStream: TStream;

    procedure LoadCoverFromDialog(TargetImage: TImage; Dialog: TOpenDialog);
  end;

implementation

{-------------------- CONSTRUCTOR --------------------}

constructor TAlbumModel.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

{-------------------- DATASET VALIDATION --------------------}

function TAlbumModel.HasValidDataset: Boolean;
begin
  Result :=
    Assigned(FDataSet) and
    FDataSet.Active and
    (not FDataSet.IsEmpty);
end;

{-------------------- COVER CHECK -------------------}

function TAlbumModel.HasCover: Boolean;
var
  Field: TField;
begin
  Result := False;
  if not HasValidDataset then Exit;

  Field := FDataSet.FindField('ALBUMCOVER');
  if (Field is TBlobField) then
    Result :=
      (not Field.IsNull) and
      (TBlobField(Field).BlobSize > 0);
end;

{-------------------- CREATE STREAM --------------------}

function TAlbumModel.CreateCoverStream: TStream;
var
  Field: TField;
begin
  Result := nil;
  if not HasValidDataset then Exit;

  Field := FDataSet.FindField('ALBUMCOVER');
  if (Field is TBlobField) and (not Field.IsNull) then
    Result := FDataSet.CreateBlobStream(Field, bmRead);
end;

{-------------------- LOAD FROM DIALOG --------------------}

procedure TAlbumModel.LoadCoverFromDialog(TargetImage: TImage; Dialog: TOpenDialog);
var
  Field: TField;
begin
  if not HasValidDataset then Exit;
  if not Assigned(TargetImage) or not Assigned(Dialog) then Exit;
  if not Dialog.Execute then Exit;

  Field := FDataSet.FindField('ALBUMCOVER');
  if not (Field is TBlobField) then Exit;

  FDataSet.Edit;
  try
    TBlobField(Field).LoadFromFile(Dialog.FileName);
    FDataSet.Post;
  except
    FDataSet.Cancel;
    raise;
  end;

  // Immediately update the image on the form
  TargetImage.Picture.LoadFromFile(Dialog.FileName);
end;

end.
