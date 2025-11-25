unit RegisterFormUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  dDatenbank, DB;

type

  { TRegisterForm }

  TRegisterForm = class(TForm)
    btnRegister: TButton;
    btnCancel: TButton;
    chkAcceptTerms: TCheckBox;
    edtEmail: TEdit;
    edtUsername: TEdit;
    edtPassword: TEdit;
    edtConfirmPassword: TEdit;
    lblRegistorFormTitle: TLabel;
    lblRegisterStatus: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
  private
    function ValidateInput: Boolean;
    function UsernameExists(const AUsername: string): Boolean;
    function GetNewUsername: string;  // getter function
  public
    property NewUsername: string read GetNewUsername;
  end;

var
  RegisterForm: TRegisterForm;

implementation

{$R *.lfm}

{---------------------------------------------------------------}
{ CANCEL BUTTON }
{---------------------------------------------------------------}
procedure TRegisterForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

{---------------------------------------------------------------}
{ REGISTER BUTTON }
{---------------------------------------------------------------}
procedure TRegisterForm.btnRegisterClick(Sender: TObject);
begin
  lblRegisterStatus.Caption := '';

  // 1 — Validate fields
  if not ValidateInput then Exit;

  // 2 — Username check from DB
  if UsernameExists(Trim(edtUsername.Text)) then
  begin
    lblRegisterStatus.Caption := 'Username already exists.';
    Exit;
  end;

  try
    // 3 — Insert using qUsersRegister (Object Inspector SQL)
    dmMain.qUsersRegister.Close;
    dmMain.qUsersRegister.Open;

    dmMain.qUsersRegister.Append;
    dmMain.qUsersRegister.FieldByName('USERNAME').AsString := Trim(edtUsername.Text);
    dmMain.qUsersRegister.FieldByName('EMAIL').AsString    := Trim(edtEmail.Text);
    dmMain.qUsersRegister.FieldByName('PWDHASH').AsString  := Trim(edtPassword.Text); // hash later
    dmMain.qUsersRegister.Post;

    lblRegisterStatus.Caption := 'Registration successful.';
    ModalResult := mrOK;

  except
    on E: Exception do
      lblRegisterStatus.Caption := 'Database error: ' + E.Message;
  end;
end;

{---------------------------------------------------------------}
{ VALIDATION LOGIC }
{---------------------------------------------------------------}
function TRegisterForm.ValidateInput: Boolean;
begin
  Result := False;

  if Trim(edtUsername.Text) = '' then
  begin
    lblRegisterStatus.Caption := 'Please enter a username.';
    Exit;
  end;

  if Trim(edtEmail.Text) = '' then
  begin
    lblRegisterStatus.Caption := 'Please enter an email.';
    Exit;
  end;

  if Trim(edtPassword.Text) = '' then
  begin
    lblRegisterStatus.Caption := 'Please enter a password.';
    Exit;
  end;

  if edtPassword.Text <> edtConfirmPassword.Text then
  begin
    lblRegisterStatus.Caption := 'Passwords do not match.';
    Exit;
  end;

  if not chkAcceptTerms.Checked then
  begin
    lblRegisterStatus.Caption := 'Please accept the terms.';
    Exit;
  end;

  Result := True;
end;

{---------------------------------------------------------------}
{ CHECK IF USERNAME EXISTS }
{---------------------------------------------------------------}
function TRegisterForm.UsernameExists(const AUsername: string): Boolean;
begin
  Result := False;
  try
    dmMain.qUsersRegister.Close;
    dmMain.qUsersRegister.Open;

    dmMain.qUsersRegister.Filter := 'USERNAME=''' + AUsername + '''';
    dmMain.qUsersRegister.Filtered := True;

    Result := not dmMain.qUsersRegister.IsEmpty;

    dmMain.qUsersRegister.Filtered := False;
    dmMain.qUsersRegister.Filter := '';

    dmMain.qUsersRegister.Close;

  except
    on E: Exception do
    begin
      lblRegisterStatus.Caption := 'DB error: ' + E.Message;
      Result := True;
    end;
  end;
end;

{---------------------------------------------------------------}
{ GETTER FUNCTION FOR NewUsername PROPERTY }
{---------------------------------------------------------------}
function TRegisterForm.GetNewUsername: string;
begin
  Result := Trim(edtUsername.Text);
end;

end.

