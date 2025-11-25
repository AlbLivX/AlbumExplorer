unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  UserObjectUnit, RegisterFormUnit, dDatenbank;

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

{ Login Button Click }
procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  User: TUserObject;
begin
  // Pass the queries from dmMain to TUserObject
  User := TUserObject.Create(dmMain.qUsersRegister, dmMain.qUsersLogin);
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

{ Register Button Click }
procedure TLoginForm.btnRegisterClick(Sender: TObject);
var
  User: TUserObject;
  RegForm: TRegisterForm;
begin
  // Pass the queries to the register form so it can create its TUserObject correctly
  RegForm := TRegisterForm.Create(Self, dmMain.qUsersRegister, dmMain.qUsersLogin);
  try
    if RegForm.ShowModal = mrOK then
    begin
      User := TUserObject.Create(dmMain.qUsersRegister, dmMain.qUsersLogin);
      try
        User.Username := RegForm.NewUsername;
        User.Email := RegForm.edtEmail.Text;
        User.Password := RegForm.edtPassword.Text;

        if User.RegisterUser then
          edtUsername.Text := User.Username
        else
          ShowMessage(User.LastError);

      finally
        User.Free;
      end;
    end;
  finally
    RegForm.Free;
  end;
end;

{ Execute Login Form }
function TLoginForm.Execute: Boolean;
begin
  FLoginSuccessful := False;
  ShowModal;
  Result := FLoginSuccessful;
end;

end.

