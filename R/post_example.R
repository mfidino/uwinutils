with_config(verbose(),{
  httr::VERB(
    verb = "POST",
    url =
      modify_url("https://us-central1-urban-wildlife-app.cloudfunctions.net/updateDetectionValStats",
                 query = "authorization=eyJhbGciOiJSUzI1NiIsImtpZCI6IjhhNjNmZTcxZTUzMDY3NTI0Y2JiYzZhM2E1ODQ2M2IzODY0YzA3ODciLCJ0eXAiOiJKV1QifQ.eyJpc3MiOiJhY2NvdW50cy5nb29nbGUuY29tIiwiYXpwIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwiYXVkIjoiODA5MzgxMTY2MzY5LXMxZXEwNTlnZmZmcnJiNWI1YXJydmVlNjY3aGk0YXN1LmFwcHMuZ29vZ2xldXNlcmNvbnRlbnQuY29tIiwic3ViIjoiMTA2MDYzMjY2MjY2NjA1NDI1NTc1IiwiZW1haWwiOiJ1cmJhbi53aWxkbGlmZS5pbnN0aXR1dGVAZ21haWwuY29tIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsImF0X2hhc2giOiJLZ2JiamJ5bUYwN2cxdWhqdUp0ZFpRIiwibmFtZSI6IlVyYmFuIFdpbGRsaWZlIEluc3RpdHV0ZSIsInBpY3R1cmUiOiJodHRwczovL2xoMy5nb29nbGV1c2VyY29udGVudC5jb20vYS0vQUF1RTdtQUR1dlJ4ODhhN0ZoLVdTeUNENEFJX3VOTGpIZG9UcXZvTTR0OD1zOTYtYyIsImdpdmVuX25hbWUiOiJVcmJhbiBXaWxkbGlmZSIsImZhbWlseV9uYW1lIjoiSW5zdGl0dXRlIiwibG9jYWxlIjoiZW4iLCJpYXQiOjE1NzIzNjE2MjksImV4cCI6MTU3MjM2NTIyOSwianRpIjoiZDczNDNmNTFkOWE4YzhmMWRkYjYzYTRlMGM3YzIwMmY2MGJlODM3MiJ9.uwD1uru7_GnVa3FeE6FStzNMYcf3TlEN83ACJcw7AUDfaHHnfQsbbiPDqhJry-ERJTWeatEVlnaMuyQqBI4yBMKOTwgJvmGUtdQyJVHDA65kg1g37z2DqkPDgZp3PzE_4lssysoPjKlaJljQUe9dg0WVlEdUcORHd2scI5FGv2Duvd6ANGzW0hxzDS8tGYu-9YcnPaDHmIMzpwCRVpzUf-ggmpF1DADdU52FLc5u5iKI5RfxpghZDdqak1MdRsIhZXeznFffSRo7sAhGN_iXXVB17gQ7qiZ9uj1kKHP8-pF74NXb3yWuRZNivkl2XKqICXKu-l7tayHNh7ErjCGYcg"),
    add_headers(`Content-Type` = "application/json",
                `cache-control` = "no-cache",
                `Accept` = "application/json"),
    body= rjson::toJSON(list(photoGroupID = groupies[i]), indent = 1),
    encode = "json")
})
