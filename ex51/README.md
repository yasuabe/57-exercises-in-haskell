# MEMO
## Firebase Set up
1. Create project
2. Create realtime database
3. Configure the Realtime Database rules as follows:
```
{
  "rules": {
    ".read": "auth != null && now < 1747234800000",
    ".write": "auth != null && now < 1747234800000"
  }
}
```
4. In the [Authentication] section, enable the email/password provider.
5. Create a user with email/password authentication.
6. Verify that the API key is properly set.

If the following API for obtaining the ID token succeeds, the setup is probably complete.
```
curl -X POST \
  -H "Content-Type: application/json" \
  -d '{
    "email": "{{ mail address}}",
    "password": "{{ password }}",
    "returnSecureToken": true
  }' \
  "https://identitytoolkit.googleapis.com/v1/accounts:signInWithPassword?key={{API KEY}}"
```

## ID token cache
Saved in the format below.
```
{
  "idToken": "{{ ID token }}",
  "refreshToken": "{{ refresh token }}",
  "expiresAt": {{ Unix time }}
}
```

## Authentication API

### signupNewUser endpoint.
https://cloud.google.com/identity-platform/docs/use-rest-api#section-create-email-password
```
curl 'https://identitytoolkit.googleapis.com/v1/accounts:signUp?key=[API_KEY]' \
-H 'Content-Type: application/json' \
--data-binary '{"email":"[user@example.com]","password":"[PASSWORD]","returnSecureToken":true}'
```
#### Sample response
NOTE: Response is in camelCase, unlike the ID token refresh API.
```
{
  "idToken": "[ID_TOKEN]",
  "email": "[user@example.com]",
  "refreshToken": "[REFRESH_TOKEN]",
  "expiresIn": "3600",
  "localId": "tRcfmLH7..."
}
```

### securetoken.googleapis.com endpoint. 
https://cloud.google.com/identity-platform/docs/use-rest-api#section-refresh-token
```
curl 'https://securetoken.googleapis.com/v1/token?key=[API_KEY]' \
-H 'Content-Type: application/x-www-form-urlencoded' \
--data 'grant_type=refresh_token&refresh_token=[REFRESH_TOKEN]'
```
#### Sample response
NOTE: Response is in snake_case, unlike the signin API.
```
{
  "expires_in": "3600",
  "token_type": "Bearer",
  "refresh_token": "[REFRESH_TOKEN]",
  "id_token": "[ID_TOKEN]",
  "user_id": "tRcfmLH7o2XrNELi...",
  "project_id": "1234567890"
}
```
