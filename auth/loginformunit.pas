unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, DBCtrls,
  dDatenbank, DB;

type

  { TLogin }

  TLogin = class(TForm)
    btnLogin: TButton;
    btnRegister: TButton;
    cbkStayLoggedIn: TCheckBox;
    edtUsername: TDBEdit;
    edtUserPassword: TDBEdit;
    lblLoginFormTitle: TLabel;
    lblLoginStatusMsg: TLabel;
    procedure btnLoginClick(Sender: TObject);


  private
      FLoginSuccessful: Boolean;
  public
     function Execute : boolean;
     property LoginSuccessful: Boolean read FLoginSuccessful;
  end;

var
  LoginForm: TLogin;

implementation

{$R *.lfm}

{ TLogin }


function TLogin.Execute: Boolean;
begin
  dmMain.cDatenbank.Connected := True;
  FLoginSuccessful := False;
  ShowModal;
  Result := FLoginSuccessful;
end;

procedure TLogin.btnLoginClick(Sender: TObject);
var
  username, password: string;
begin
  username := Trim(edtUsername.Text);
  password := Trim(edtUserPassword.Text);

  if (username = '') or (password = '') then
  begin
    lblLoginStatusMsg.Caption := 'Please enter username and password.';
    Exit;
  end;

  if username = 'Alice' then
  begin
    if password = '1234' then
    begin
      FLoginSuccessful := True;
      lblLoginStatusMsg.Caption := 'Logging in...';
      if cbkStayLoggedIn.Checked then
        lblLoginStatusMsg.Caption := lblLoginStatusMsg.Caption + ' (Will stay logged in)';

      Close; // closes form, Execute returns True
    end
    else
    begin
      FLoginSuccessful := False;
      lblLoginStatusMsg.Caption := 'Wrong password.';
    end;
  end
  else
  begin
    FLoginSuccessful := False;
    lblLoginStatusMsg.Caption := 'Account does not exist.';
  end;
end;
end.

