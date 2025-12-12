unit LoginFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Dialogs,
  dDatenbank, UserObjectUnit, RegisterFormUnit;

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

procedure TLoginForm.btnLoginClick(Sender: TObject);
var
  User: TUserObject;
begin
  // Create user object using the queries from dmMain
  User := TUserObject.Create(dmMain.qUserCheckExists,
                             dmMain.qUsersInsert,
                             dmMain.qUsersLogin);
  try
    if User.ValidateCredentials(edtUsername.Text, edtUserPassword.Text) then
    begin
      FLoginSuccessful := True;
      lblLoginStatusMsg.Caption := 'Login successful';

      // Store logged-in user ID
      dmMain.CurrentUserID := dmMain.qUsersLogin.FieldByName('ID').AsInteger;

      // Open album dataset filtered by user
      dmMain.qAlbum.Close;
      dmMain.qAlbum.ParamByName('UID').AsInteger := dmMain.CurrentUserID;
      dmMain.qAlbum.ParamByName('SEARCH').AsString := '%';
      dmMain.qAlbum.Open;

      // Open songs dataset
      if not dmMain.qSongs.Active then
        dmMain.qSongs.Open;

      // Save settings if "Stay logged in" checked
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

procedure TLoginForm.btnRegisterClick(Sender: TObject);
var
  RegForm: TRegisterForm;
begin
  RegForm := TRegisterForm.Create(Self,
                                   dmMain.qUserCheckExists,
                                   dmMain.qUsersInsert,
                                   dmMain.qUsersLogin);
  try
    if RegForm.ShowModal = mrOK then
      edtUsername.Text := RegForm.NewUsername;
  finally
    RegForm.Free;
  end;
end;

function TLoginForm.Execute: Boolean;
begin
  FLoginSuccessful := False;
  ShowModal;
  Result := FLoginSuccessful;
end;

end.

