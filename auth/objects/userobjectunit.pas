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

    FQueryCheckExists: TUniQuery; // SELECT check
    FQueryInsert:      TUniQuery;      // INSERT
    FQueryLogin:       TUniQuery;       // SELECT for login

    procedure SetPassword(const APassword: string);
    procedure HandleDatabaseError(const E: Exception);
  public
    constructor Create(AQueryCheck, AQueryInsert, AQueryLogin: TUniQuery);

    property Username: string read FUsername write FUsername;
    property Email: string read FEmail write FEmail;
    property Password: string write SetPassword;
    property StayLoggedIn: Boolean read FStayLoggedIn write FStayLoggedIn;
    property LastError: string read FLastError;

    function ValidateCredentials(const AUsername, APassword: string): Boolean;
    function RegisterUser: Boolean;

    procedure LoadSettings;
    procedure SaveSettings;
  end;

implementation

{ Constructor }
constructor TUserObject.Create(AQueryCheck, AQueryInsert, AQueryLogin: TUniQuery);
begin
  inherited Create;
  FUsername := '';
  FEmail := '';
  FPassword := '';
  FStayLoggedIn := False;
  FLastError := '';
  FQueryCheckExists := AQueryCheck;
  FQueryInsert := AQueryInsert;
  FQueryLogin := AQueryLogin;
end;

{ Password Setter }
procedure TUserObject.SetPassword(const APassword: string);
begin
  FPassword := APassword; // Plain text for now
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

{ Centralized error handling }
procedure TUserObject.HandleDatabaseError(const E: Exception);
begin
  FLastError := 'DB error: ' + E.Message;
end;

{ Register user in DB }
function TUserObject.RegisterUser: Boolean;
begin
  Result := False;
  FLastError := '';
  try
    // --- Check for existing username/email ---
    FQueryCheckExists.Close;
    FQueryCheckExists.ParamByName('USERNAME').AsString := Trim(FUsername);
    FQueryCheckExists.ParamByName('EMAIL').AsString := Trim(FEmail);
    FQueryCheckExists.Open;

    if not FQueryCheckExists.IsEmpty then
    begin
      FLastError := 'Username or email already exists';
      Exit(False);
    end;
    FQueryCheckExists.Close;

    // --- Insert user (ID auto-assigned by DB) ---
    FQueryInsert.Close;
    FQueryInsert.ParamByName('USERNAME').AsString := Trim(FUsername);
    FQueryInsert.ParamByName('EMAIL').AsString := Trim(FEmail);
    FQueryInsert.ParamByName('PWDHASH').AsString := Trim(FPassword);
    FQueryInsert.ExecSQL;

    // Commit (dmMain.cDatenbank is your TUniConnection)
    dmMain.cDatenbank.Commit;
    Result := True;
  except
    on E: Exception do
    begin
      HandleDatabaseError(E);
      Result := False;
    end;
  end;
end;

{ Validate credentials (Option B) }
function TUserObject.ValidateCredentials(const AUsername, APassword: string): Boolean;
var
  StoredHash: string;
begin
  Result := False;
  FLastError := '';
  try
    FQueryLogin.Close;
    FQueryLogin.ParamByName('USERNAME').AsString := Trim(AUsername);
    FQueryLogin.Open;

    if FQueryLogin.IsEmpty then
    begin
      FLastError := 'User not found';
      Exit(False);
    end;

    StoredHash := FQueryLogin.FieldByName('PWDHASH').AsString;

    if StoredHash = APassword then
      Result := True
    else
      FLastError := 'Invalid password';
  except
    on E: Exception do
    begin
      HandleDatabaseError(E);
      Result := False;
    end;
  end;
end;

end.

