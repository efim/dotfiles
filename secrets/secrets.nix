let
  efim-at-work-laptop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA+xChj0qS5VBFtGO3Kg1MhTeqPq/x3DQGDzlm1c6QP8 efim.wool@gmail.com";

  users = [ efim-at-work-laptop ];

  franzk = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID/jAjOzXUMarxSkGlXAyoQEQ9xuA7zePT4ZnBQWT4vX";

  machines = [ franzk ];
in
{
  "email-pass.age".publicKeys = users;
  "franzk-server-secret.age".publicKeys = users ++ [ franzk ];
}
