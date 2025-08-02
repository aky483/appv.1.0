from google_auth_oauthlib.flow import InstalledAppFlow
import pickle

SCOPES = ['https://mail.google.com/']
flow = InstalledAppFlow.from_client_secrets_file('credentials.json', SCOPES)
creds = flow.run_local_server(port=0)

with open('gmail_token.pkl', 'wb') as token:
    pickle.dump(creds, token)

print("✅ Gmail OAuth token saved as gmail_token.pkl")