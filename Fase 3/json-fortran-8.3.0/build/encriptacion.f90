module password_module
    implicit none
    contains

    function encrypt_password(password, key) result(encrypt_password_)
        character(len=*), intent(in) :: password
        integer, intent(in) :: key
        character(len=len(password)) :: encrypt_password_
        integer :: i

        do i = 1, len(password)
            encrypt_password_(i:i) = char(iachar(password(i:i)) + key)
        end do
    end function encrypt_password

    function decrypt_password(encrypted_password, key) result(decrypt_password_)
        character(len=*), intent(in) :: encrypted_password
        integer, intent(in) :: key
        character(len=len(encrypted_password)) :: decrypt_password_
        integer :: i

        do i = 1, len(encrypted_password)
            decrypt_password_(i:i) = char(iachar(encrypted_password(i:i)) - key)
        end do
    end function decrypt_password

    function check_password(password, encrypted_password, key) result(check_password_)
        character(len=*), intent(in) :: password, encrypted_password
        integer, intent(in) :: key
        logical :: check_password_

        check_password_ = (password == decrypt_password(encrypted_password, key))
    end function check_password

end module password_module
