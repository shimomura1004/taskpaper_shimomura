#!/usr/bin/env ruby
today = ARGV[0] || Time.now.strftime("%Y-%m-%d")

File::open("#{ENV['HOME']}/.todo/todo.taskpaper",
           :encoding => Encoding::Shift_JIS) do |file|
  lines = file.readlines

  sum = Hash.new 0
  lines.select{|l| l.match /^\s*-.*@wl.*$/}.each do |task|
    is_done         = task.match(Regexp.new("@done\\(#{today}\\)"))
    workload        = task.match(/@wl\(([0-9]+)\)/)
    actual_workload = task.match(/@a\(([0-9]+)\)/)
    inprogress      = task.match(/@inprogress/)

    sum[:total] += workload[1].to_i if inprogress
    if is_done
      sum[:done] += workload[1].to_i
      if actual_workload
        sum[:actual_done] += actual_workload[1].to_i
      else
        sum[:actual_done] += workload[1].to_i
      end
    end
  end

  puts "InProgress : #{sum[:total]} pt"
  puts "Done(f/a)  : #{sum[:done]}/#{sum[:actual_done]} pt"
end
