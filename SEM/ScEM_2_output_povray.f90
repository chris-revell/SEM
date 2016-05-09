!CK Revell, February 2016

module scem_2_output_povray

  use scem_0_arrays
  use scem_0_input
  use scem_0_useful
  use scem_1_types

  implicit none

  contains

    subroutine scem_output_povray

      character(len=24)	    :: povray_filename  !Filename for data output
      real*8                :: sphere_radius    !Radius of sphere used to represent cell volume in povray visualiation. Calculated from cell volume.
      integer               :: corner_element   !Label of element forming corner of smoothed_triangle
      real*8, dimension(3)  :: corner           !Array to store corner vector used in smoothed_triangle data output
      real*8, dimension(3)  :: normal           !Array to store normal vector used in smoothed_triangle data output
      integer               :: label1           !Useful for abbreviating long expressions
      integer               :: label2

      !Create filename for povray output file.
      write(povray_filename,"(A18,I2.2,A4)") "/povray_data/snap_", n_snapshots, ".pov"

      !Open file for povray output
      open(unit=42, file=output_folder//povray_filename,status='unknown')

      write(42,*) '#version 3.5;'
      write(42,*) '#include "colors.inc"'
      write(42,*) '#include "textures.inc"'
      write(42,*) 'background {White}'
      write(42,*)
      write(42,*) 'camera {'
      write(42,*) '   location  <500, 0, 0>'
      write(42,*) '   angle 12'
      write(42,*) '   look_at<0,0,0>}'
      write(42,*)
      write(42,*) 'light_source { < -60, 60, 0 > color White }'
      write(42,*) 'light_source { < 60, -60, 0 > color White }'
      write(42,*) 'light_source { < 0, 0, 60 > color White }'
      write(42,*) 'light_source { < 0, 0, -60 > color White }'
      write(42,*)

      if (flag_povray_elements.EQ.1) then
        !Draw spheres for all elements of all cells in the system, coloured according to element type
        do i=1, ne
          if ((elements(i)%type).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A60,I2.2)') ' sphere {  < ',&
                elements(i)%position(1), ',', elements(i)%position(2),',', &
                elements(i)%position(3),&
                '> 1.5 texture { pigment { color Green } } } // element, cell',&
                elements(i)%parent
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A58,I2.2)') ' sphere {  < ',&
                elements(i)%position(1), ',', elements(i)%position(2),',', &
                elements(i)%position(3),&
                '> 1.5 texture { pigment { color Red } } } // element, cell',&
                elements(i)%parent
          endif
        enddo
      endif
      write(42,*)

      if (flag_povray_pairs.EQ.1) then
        !Draw cylinders for inter-element pair interactions. Inter-cell pairs black, intra-cell pairs blue.
        do j=1, np
          m = pairs(j,1)
          n = pairs(j,2)
          k = elements(m)%parent
          l = elements(n)%parent

          if (k.NE.l) then !Elements are not in the same cell
            !Inter-cell interactions in black
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                          F18.14,A62,I2.2,A7,I2.2)') &
                          ' cylinder {  < ', &
                          elements(m)%position(1), ',', &
                          elements(m)%position(2), ',', &
                          elements(m)%position(3), '>, <', &
                          elements(n)%position(1), ',', &
                          elements(n)%position(2), ',', &
                          elements(n)%position(3), &
                          '> 0.5 texture { pigment { color Black } } } // pair inter cell',&
                          elements(m)%parent, ' , cell',&
                          elements(n)%parent


            !Intra-cell cortex pair interactions in red
  !          elseif((elements(pairs(j,1))%type.EQ.2).AND.(elements(pairs(j,2))%type.EQ.2)) then
  !            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
  !                          F18.14,A50,I2.2,A8,I2.2)') &
  !                          ' cylinder {  < ', &
  !                          elements(pairs(j,1))%position(1), ',', &
  !                          elements(pairs(j,1))%position(2), ',', &
  !                          elements(pairs(j,1))%position(3), '>, <', &
  !                          elements(pairs(j,2))%position(1), ',', &
  !                          elements(pairs(j,2))%position(2), ',', &
  !                          elements(pairs(j,2))%position(3), &
  !                          '> 0.5 texture { pigment { color Red } } } // cell ',&
  !                          elements(pairs(j,1))%parent, ' , cell ',&
  !                          elements(pairs(j,2))%parent
          else !Elements are in the same cell
            !All intra-cell interactions in blue
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                            F18.14,A61,I2.2,A7,I2.2)') &
                            ' cylinder {  < ', &
                            elements(pairs(j,1))%position(1), ',', &
                            elements(pairs(j,1))%position(2), ',', &
                            elements(pairs(j,1))%position(3), '>, <', &
                            elements(pairs(j,2))%position(1), ',', &
                            elements(pairs(j,2))%position(2), ',', &
                            elements(pairs(j,2))%position(3), &
                            '> 0.5 texture { pigment { color Blue } } } // pair intra cell',&
                            elements(pairs(j,1))%parent, ' , cell',&
                            elements(pairs(j,2))%parent
          endif
        enddo
      endif
      write(42,*)

      !Write cell position data to file in povray format
      if (flag_povray_volumes.EQ.1) then
        !Draw spheres for all cells in the system, coloured according to cell type, with transparency set to 0.66 and phong set to 0.8
        do i=1, nc
          sphere_radius = (3.0*cells(i)%volume/(pi*4.0))**(1.0/3.0)     !Radius is cube root of (3*volume/4pi)
          if ((cells(i)%fate).EQ.1) then
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A81,I2.2)') &
                  ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                  ',', cells(i)%position(3), '> ', sphere_radius,&
                  ' texture { pigment { color Green transmit .66}finish{phong .8} } } // volume cell', &
                  cells(i)%label
          else
            write(42,'(A12,F18.14,A2,F18.14,A2,F18.14,A2,F18.14,A79,I2.2)') &
                  ' sphere {  < ', cells(i)%position(1), ',', cells(i)%position(2), &
                  ',', cells(i)%position(3), '> ', sphere_radius,&
                  ' texture { pigment { color Red transmit .66}finish{phong .8} } } // volume cell', &
                  cells(i)%label
          endif
        enddo
        write(42,*)
      endif

      !Write cell surface triangles to file in povray smoothed_triangle format
      if (flag_povray_triangles.EQ.1) then
        do i=1, nc                                                !Loop over all cells
          do j=1, cells(i)%triplet_count                          !Loop over all delaunay triangles within the cell
            write(42,'(A17)',advance='no') "smooth_triangle {"    !Begin writing data structure for smoothed triangle for each delaunay triangle
            do k=1, 3                                             !Loop over all element in Delaunay triangle
              corner_element = cells(i)%triplets(k,j)
              corner(:) = elements(corner_element)%position(:)    !Calculate corner and normal for the element
              normal(:) = corner(:)-cells(i)%position(:)
              write(42,'(A1,F18.14,A1,F18.14,A1,F18.14,A2)',advance='no') "<", &
                  corner(1), ',', corner(2), ',', corner(3), '>,'  !Write corner and normal to file
              write(42,'(A1,F18.14,A1,F18.14,A1,F18.14,A1)',advance='no') "<", &
                  normal(1), ',', normal(2), ',', normal(3), '>'
              if (k.LT.3) then
                write(42,'(A1)',advance='no') ","
              else
                EXIT
              endif
            enddo
            write(42,"(A23)",advance='no') " texture{pigment{color "
            if (cells(i)%fate.EQ.1) then
              write(42,'(A25,I2.2)') "Green}}} // triangle cell", cells(i)%label
            else
              write(42,'(A23,I2.2)') "Red}}} // triangle cell", cells(i)%label
            endif
          enddo
        enddo
      endif
      write(42,*) ""

      !Write commands to draw Delaunay cortex interactions to file.
      if (flag_povray_cortex_pairs.EQ.1) then
        do i=1, nc  !Loop over all cells
          do j=1, cells(i)%triplet_count  !Loop over all Delaunay triangles in each cell
            label1 = cells(i)%triplets(1,j) !Labels for adjacent elements in this triplet of the triangulation
            label2 = cells(i)%triplets(2,j)
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                            F18.14,A61,I2.2)') &
                            ' cylinder {  < ', &
                            elements(label1)%position(1), ',', &
                            elements(label1)%position(2), ',', &
                            elements(label1)%position(3), '>, <', &
                            elements(label2)%position(1), ',', &
                            elements(label2)%position(2), ',', &
                            elements(label2)%position(3), &
                            '> 0.5 texture { pigment { color Red } } } // cortex pair cell',&
                            elements(label1)%parent !Both elements are in the same cell so we only need one label
            label1 = cells(i)%triplets(2,j) !Labels for adjacent elements in this triplet of the triangulation
            label2 = cells(i)%triplets(3,j)
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                            F18.14,A61,I2.2)') &
                            ' cylinder {  < ', &
                            elements(label1)%position(1), ',', &
                            elements(label1)%position(2), ',', &
                            elements(label1)%position(3), '>, <', &
                            elements(label2)%position(1), ',', &
                            elements(label2)%position(2), ',', &
                            elements(label2)%position(3), &
                            '> 0.5 texture { pigment { color Red } } } // cortex pair cell',&
                            elements(label1)%parent !Both elements are in the same cell so we only need one label
            label1 = cells(i)%triplets(3,j) !Labels for adjacent elements in this triplet of the triangulation
            label2 = cells(i)%triplets(1,j)
            write(42,'(A14,F18.14,A2,F18.14,A2,F18.14,A4,F18.14,A2,F18.14,A2,&
                            F18.14,A61,I2.2)') &
                            ' cylinder {  < ', &
                            elements(label1)%position(1), ',', &
                            elements(label1)%position(2), ',', &
                            elements(label1)%position(3), '>, <', &
                            elements(label2)%position(1), ',', &
                            elements(label2)%position(2), ',', &
                            elements(label2)%position(3), &
                            '> 0.5 texture { pigment { color Red } } } // cortex pair cell',&
                            elements(label1)%parent !Both elements are in the same cell so we only need one label
          enddo
        enddo
      endif

      close(unit=42)

    end subroutine scem_output_povray

end module scem_2_output_povray
