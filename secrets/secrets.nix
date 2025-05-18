let
  efim-at-work-laptop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA+xChj0qS5VBFtGO3Kg1MhTeqPq/x3DQGDzlm1c6QP8 efim.wool@gmail.com";
  efim-at-framework = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKDpGmVF5+Yk/zZfxkaMqdZa0O+3XXFzKsdOqV64Q+/M efim.wool@gmail.com";

  users = [ efim-at-work-laptop efim-at-framework ];

  franzk = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID/jAjOzXUMarxSkGlXAyoQEQ9xuA7zePT4ZnBQWT4vX";

  machines = [ franzk ];
in
{
  "email-pass.age".publicKeys = users;
  "franzk-server-secret.age".publicKeys = users ++ [ franzk ];
  "server-user-password.age".publicKeys = users ++ machines;
  "radicale-user.age".publicKeys = users ++ [ franzk ];
  "example-secret.age".publicKeys = users ++ machines;
}
