unit UserObjectUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, DB, Uni, dDatenbank;

const
  C_INI_FILE = 'user.ini';
  C_SECTION  = 'Login';

type
  { TUserObject: Encapsulates all user-related operations }
  TUserObject = class
  private
    FUsername: string;
    FEmail: string;
    FPassword: string;
    FStayLoggedIn: Boolean;
    FLastError: string;

    FQueryRegister: TUniQuery;
    FQueryLogin: TUniQuery;

    procedure SetPassword(const APassword: string);
  public
    constructor Create(AQueryRegister, AQueryLogin: TUniQuery);

    property Username: string read FUsername write FUsername;
    property Email: string read FEmail write FEmail;
    property Password: string write SetPassword;
    property StayLoggedIn: Boolean read FStayLoggedIn write FStayLoggedIn;
    property LastError: string read FLastError;

    function ValidateCredentials(const AUsername, APassword: string): Boolean;
    function UsernameExists(const AUsername: string): Boolean;
    function RegisterUser: Boolean;

    procedure LoadSettings;
    procedure SaveSettings;
  end;

implementation

{ Constructor }
constructor TUserObject.Create(AQueryRegister, AQueryLogin: TUniQuery);
begin
  inherited Create;
  FUsername := '';
  FEmail := '';
  FPassword := '';
  FStayLoggedIn := False;
  FLastError := '';
  FQueryRegister := AQueryRegister;
  FQueryLogin := AQueryLogin;
end;

{ Password Setter }
procedure TUserObject.SetPassword(const APassword: string);
begin
  FPassword := APassword; // plain text for now, hash later
end;

{ Load settings from INI }
procedure TUserObject.LoadSettings;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    FUsername := Sett.ReadString(C_SECTION, 'Username', '');
    FStayLoggedIn := Sett.ReadBool(C_SECTION, 'StayLoggedIn', False);
  finally
    Sett.Free;
  end;
end;

{ Save settings to INI }
procedure TUserObject.SaveSettings;
var
  Sett: TIniFile;
begin
  Sett := TIniFile.Create(C_INI_FILE);
  try
    Sett.WriteString(C_SECTION, 'Username', FUsername);
    Sett.WriteBool(C_SECTION, 'StayLoggedIn', FStayLoggedIn);
  finally
    Sett.Free;
  end;
end;

{ Check if username exists in DB }
function TUserObject.UsernameExists(const AUsername: string): Boolean;
begin
  Result := False;
  FLastError := '';
  try
    FQueryRegister.Close;
    FQueryRegister.SQL.Text := 'SELECT ID FROM USERS WHERE USERNAME = :USERNAME';
    FQueryRegister.ParamByName('USERNAME').AsString := AUsername;
    FQueryRegister.Open;

    Result := not FQueryRegister.IsEmpty;
    FQueryRegister.Close;
  except
    on E: Exception do
    begin
      FLastError := 'DB error: ' + E.Message;
      Result := True; // assume exists to prevent duplicate insert
    end;
  end;
end;

{ Register user in DB using ExecSQL }
function TUserObject.RegisterUser: Boolean;
var
  NewID: Integer;
begin
  Result := False;
  FLastError := '';
  try
    // Get next ID from generator
    dmMain.qNextUserID.Connection := dmMain.cDatenbank;
    dmMain.qNextUserID.Open;
    NewID := dmMain.qNextUserID.FieldByName('NEWID').AsInteger;
    dmMain.qNextUserID.Close;

    // Insert new user via ExecSQL
    FQueryRegister.Close;
    FQueryRegister.SQL.Text := 'INSERT INTO USERS (ID, USERNAME, EMAIL, PWDHASH) ' +
                               'VALUES (:ID, :USERNAME, :EMAIL, :PWDHASH)';
    FQueryRegister.ParamByName('ID').AsInteger := NewID;
    FQueryRegister.ParamByName('USERNAME').AsString := Trim(FUsername);
    FQueryRegister.ParamByName('EMAIL').AsString := Trim(FEmail);
    FQueryRegister.ParamByName('PWDHASH').AsString := Trim(FPassword);
    FQueryRegister.ExecSQL;

    // Commit transaction
    dmMain.cDatenbank.Commit;

    Result := True;
  except
    on E: Exception do
      FLastError := 'DB error: ' + E.Message;
  end;
end;

{ Validate credentials }
function TUserObject.ValidateCredentials(const AUsername, APassword: string): Boolean;
var
  StoredHash: string;
begin
  Result := False;
  FLastError := '';

  try
    FQueryLogin.Close;
    FQueryLogin.SQL.Text := 'SELECT PWDHASH FROM USERS WHERE USERNAME = :USERNAME';
    FQueryLogin.ParamByName('USERNAME').AsString := AUsername;
    FQueryLogin.Open;

    if FQueryLogin.IsEmpty then
    begin
      FLastError := 'User not found';
      Exit;
    end;

    StoredHash := FQueryLogin.FieldByName('PWDHASH').AsString;

    if StoredHash = APassword then
      Result := True
    else
      FLastError := 'Invalid password';

  except
    on E: Exception do
      FLastError := 'Error validating user: ' + E.Message;
  end;
end;

end.

