unit BLOBImageStreamTest;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ExtDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    procedure LoadImageFromDialog;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ -------------------- INIT -------------------- }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Cursor := crHandPoint; // indicate click
end;

{ -------------------- CLICK EVENT -------------------- }
procedure TForm1.Image1Click(Sender: TObject);
begin
  LoadImageFromDialog;
end;

{ -------------------- LOAD IMAGE -------------------- }
procedure TForm1.LoadImageFromDialog;
begin
  if OpenPictureDialog1.Execute then
  begin
    try
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    except
      on E: Exception do
        ShowMessage('Failed to load image: ' + E.Message);
    end;
  end;
end;

end.

