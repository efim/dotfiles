let
  efim-at-work-laptop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIA+xChj0qS5VBFtGO3Kg1MhTeqPq/x3DQGDzlm1c6QP8 efim.wool@gmail.com";

  users = [ efim-at-work-laptop ];
in
{
  "email-pass.age".publicKeys = users;
}
