  ! A Fortran programme to generate possible win conditions for a Splatfest.

program splatfest
  implicit none

  integer :: i ! counter and placeholder
  integer :: j ! counter and placeholder
  integer :: k ! counter and placeholder
  integer, dimension(5) :: holder ! Hold integer values of win conditions
  integer, dimension(243, 5) :: combos ! Combination of wins and losses
  integer, dimension(3) :: points(0:2) ! # of points each team gets. This one ranges from 0 to 2 to make the maths easier.
  integer, dimension(243, 3) :: allpoints ! Every combination of points

  ! Each element of combos records whether your chosen idol has won or lost that category. A win is represented with TRUE and a loss with FALSE.
  ! combos(1): Sneak Peak
  ! combos(2): Popularity
  ! combos(3): Open
  ! combos(4): Pro
  ! combos(5) : Tricolor

  ! Now we need to generate every possible combination of values for combos, except all-zeroes
  ! I tried to be clever with rotational symmetry, but it turns out that doesn't offer anything over brute force

  combos = 0
  holder = 0

  do i=2,243
     do j = 1,5
        holder(j) = mod((holder(j) + 1), 3)
        if (holder(j) > 0) then
           exit
        end if
     end do
     combos(i,:) = holder(:)
  end do

  ! Combos should now contain every possible combination of wins and losses.
  ! Let's look at some point combos.

  do i = 1,243
     points = 0
     points(combos(i,1)) = points(combos(i,1)) + 7 ! Sneak Peak
     points(combos(i,2)) = points(combos(i,2)) + 8 ! Popularity
     points(combos(i,3)) = points(combos(i,3)) + 12 ! Open
     points(combos(i,4)) = points(combos(i,4)) + 12 ! Pro
     points(combos(i,5)) = points(combos(i,5)) + 15 ! Tricolor

     allpoints(i,:) = points
  end do
  
  ! So now allpoints has every possible point combo.

  ! How many ways can you win the Splatfest but lose Tricolor?
  ! Let's say our team is #0 to make things easier.
  do i = 1,243
     if ((allpoints(i,1) > allpoints(i,2)) .and. (allpoints(i,1) > allpoints(i,3))) then
        ! I know maxval is the right thing to do here but it doesn't work for me.
        ! Team 0 wins the Splatfest
        if (combos(i,5) .ne. 0) then
           ! Team 0 loses Tricolor
           print *, combos(i,:), "You won the Splatfest and LOST the Tricolor category"
        else
           ! Team 0 wins Tricolor
           print *, combos(i,:), "You won the Slatfest and the WON the Tricolor category"
        end if
     else
        ! Team 0 loses the Splatfest
        if (combos(i,5) == 0) then
           ! Team 0 wins Tricolor
           print *, combos(i,:), "Too bad, but at least you TOOK Tricolor"
        else
           ! Team 0 loses the Splatfest and the Tricolor
           print *, combos(i,:), "YOU LOSE you loser you!"
        end if
     end if
  end do

       
end program splatfest
