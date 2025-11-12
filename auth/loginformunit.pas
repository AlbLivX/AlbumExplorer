unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, dDatenBank;

type

  { TLogin }

  TLogin = class(TForm)
    btnLogin: TButton;
    btnRegister: TButton;
    cbkStayLoggedIn: TCheckBox;
    edtUsername: TEdit;
    edtUserPassword: TEdit;
    lblLoginFormTitle: TLabel;
    lblLoginStatusMsg: TLabel;
    procedure btnLoginClick(Sender: TObject);

  private
    FLoginSuccessful: Boolean;
  public
    property LoginSuccessful: Boolean read FLoginSuccessful;
  end;

var
  LoginForm: TLogin;

implementation

{$R *.lfm}

{ TLogin }


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
      lblLoginStatusMsg.Caption := 'Logging in...';
      FLoginSuccessful := True;

      if cbkStayLoggedIn.Checked then
      begin
        lblLoginStatusMsg.Caption := lblLoginStatusMsg.Caption + ' (Will stay logged in)';
      end;
    end
    else
    begin
      FLoginSuccessful := False;
      lblLoginStatusMsg.Caption := 'Wrong password.';
      Exit;
    end;
  end
  else
  begin
    FLoginSuccessful := False;
    lblLoginStatusMsg.Caption := 'Account does not exist.';
    Exit;
  end;
  Close;
end;


end.

