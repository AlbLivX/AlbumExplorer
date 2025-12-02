unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs, UserObjectUnit, RegisterFormUnit, dDatenbank, Uni;

type
  { TLoginForm }
  TLoginForm = class(TForm)
    edtUsername: TEdit;
    edtUserPassword: TEdit;
    cbkStayLoggedIn: TCheckBox;
    btnLogin: TButton;
    btnRegister: TButton;
    lblLoginStatusMsg: TLabel;

    procedure btnLoginClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
  private
    FLoginSuccessful: Boolean;
  public
    function Execute: Boolean;
    property LoginSuccessful: Boolean read FLoginSuccessful;
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.lfm}

{ Login button }
procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  User: TUserObject;
begin
  // Create with 3 queries: checkExists, insert, login
  User := TUserObject.Create(dmMain.qUserCheckExists, dmMain.qUsersInsert, dmMain.qUsersLogin);
  try
    if User.ValidateCredentials(edtUsername.Text, edtUserPassword.Text) then
    begin
      FLoginSuccessful := True;
      lblLoginStatusMsg.Caption := 'Login successful';
      if cbkStayLoggedIn.Checked then
      begin
        User.Username := edtUsername.Text;
        User.StayLoggedIn := True;
        User.SaveSettings;
      end;
      Close;
    end
    else
    begin
      FLoginSuccessful := False;
      lblLoginStatusMsg.Caption := User.LastError;
    end;
  finally
    User.Free;
  end;
end;

{ Register button }
procedure TLoginForm.btnRegisterClick(Sender: TObject);
var
  RegForm: TRegisterForm;
begin
  // Pass the same three queries to the register form
  RegForm := TRegisterForm.Create(Self, dmMain.qUserCheckExists, dmMain.qUsersInsert, dmMain.qUsersLogin);
  try
    if RegForm.ShowModal = mrOK then
      edtUsername.Text := RegForm.NewUsername;
  finally
    RegForm.Free;
  end;
end;

{ Execute login form }
function TLoginForm.Execute: Boolean;
begin
  FLoginSuccessful := False;
  ShowModal;
  Result := FLoginSuccessful;
end;

end.

